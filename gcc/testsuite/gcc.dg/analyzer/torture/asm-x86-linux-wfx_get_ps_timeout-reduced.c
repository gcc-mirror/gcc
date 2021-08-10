/* { dg-do assemble { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */

/* Reproducer for false positive from -Wanalyzer-null-dereference seen
   in Linux kernel (drivers/staging/wfx/sta.c; GPL-2.0) due to
   the analyzer not grokking that array_index_mask_nospec is
   effectively pure, and thus not realizing that array_index_no_spec
   is also pure, leading to wdev_to_wvif not being treated as pure,
   and thus able to return non-NULL and then NULL.  */

typedef unsigned char u8;
#define NULL ((void *)0)

/* Types.  */

struct ieee80211_vif {
  int placeholder;
  /* snip */
  u8 drv_priv[];
};

struct wfx_dev {
  /* snip */
  struct ieee80211_vif *vif[2];
  /* snip */
};

struct wfx_vif {
  struct wfx_dev *wdev;
  struct ieee80211_vif *vif;
  /* snip */
};

/* Copied from arch/x86/include/asm/barrier.h */

static inline unsigned long array_index_mask_nospec(unsigned long index,
		unsigned long size)
{
	unsigned long mask;

	asm volatile ("cmp %1,%2; sbb %0,%0;"
			:"=r" (mask)
			:"g"(size),"r" (index)
			:"cc");
	return mask;
}

/* Simplified from include/linux/kernel.h */
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

/* Simplified from include/linux/nospec.h */

#define array_index_nospec(index, size)					\
({									\
	typeof(index) _i = (index);					\
	typeof(size) _s = (size);					\
	unsigned long _mask = array_index_mask_nospec(_i, _s);		\
	/* snip */							\
	(typeof(_i)) (_i & _mask);					\
})

/* Simplified from drivers/staging/wfx/wfx.h */

static inline struct wfx_vif *wdev_to_wvif(struct wfx_dev *wdev, int vif_id) {
  vif_id = array_index_nospec(vif_id, ARRAY_SIZE(wdev->vif));
  if (!wdev->vif[vif_id]) {
    return NULL;
  }
  return (struct wfx_vif *)wdev->vif[vif_id]->drv_priv;
}

struct ieee80211_vif *test (struct wfx_vif *wvif) {
  if (wdev_to_wvif(wvif->wdev, 1))
    return wdev_to_wvif(wvif->wdev, 1)->vif; /* { dg-bogus "dereference of NULL" } */
  else
    return NULL;
}
