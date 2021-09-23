/* { dg-do assemble { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-fsanitize=bounds -fno-analyzer-call-summaries" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

/* Reduced from linux kernel: drivers/staging/wfx/sta.c (GPL-2.0)
   on x86_64 with "allyesconfig"

   This test is deliberately not fully reduced, as an integration test
   that the analyzer doesn't emit bogus "dereference of NULL" warnings
   on the repeated wdev_to_wvif calls.  */

#define NULL ((void *)0)

/* Types.  */

typedef unsigned char __u8;
typedef unsigned short __u16;
__extension__ typedef unsigned long long __u64;

typedef __u8 u8;
typedef __u16 u16;
typedef __u64 u64;

enum { false = 0, true = 1 };
typedef _Bool bool;

struct device;

typedef struct {
  int counter;
} atomic_t;

struct static_key {
  atomic_t enabled;
  union {
    unsigned long type;
    struct jump_entry *entries;
    struct static_key_mod *next;
  };
};

struct static_key_true {
  struct static_key key;
};

struct static_key_false {
  struct static_key key;
};

struct _ddebug {
  const char *modname;
  const char *function;
  const char *filename;
  const char *format;
  unsigned int lineno : 18;
  unsigned int flags : 8;

  union {
    struct static_key_true dd_key_true;
    struct static_key_false dd_key_false;
  } key;

} __attribute__((aligned(8)));

enum nl80211_iftype {
  /* [...snip...] */
  NL80211_IFTYPE_AP,
  /* [...snip...] */
  NUM_NL80211_IFTYPES,
  NL80211_IFTYPE_MAX = NUM_NL80211_IFTYPES - 1
};

struct ieee80211_channel {
  /* [...snip...] */
  u16 hw_value;
  /* [...snip...] */
};

struct cfg80211_chan_def {
  struct ieee80211_channel *chan;
  /* [...snip...] */
};

struct ieee80211_bss_conf {
  /* [...snip...] */
  bool assoc, ibss_joined;
  /* [...snip...] */
  struct cfg80211_chan_def chandef;
  /* [...snip...] */
  bool ps;
  /* [...snip...] */
};

struct ieee80211_conf {
  /* [...snip...] */
  int power_level, dynamic_ps_timeout;
  /* [...snip...] */
};

struct ieee80211_vif {
  enum nl80211_iftype type;
  struct ieee80211_bss_conf bss_conf;
  /* [...snip...] */
  u8 drv_priv[] __attribute__((__aligned__(sizeof(void *))));
};

struct ieee80211_hw {
  struct ieee80211_conf conf;
  /* [...snip...] */
};

struct wfx_dev {
  /* [...snip...] */
  struct device *dev;
  struct ieee80211_hw *hw;
  struct ieee80211_vif *vif[2];
  /* [...snip...] */
  int force_ps_timeout;
};

struct wfx_vif {
  struct wfx_dev *wdev;
  struct ieee80211_vif *vif;
  /* [...snip...] */
};

/* Function decls.  */

extern __attribute__((__format__(printf, 1, 2))) void
__warn_printk(const char *fmt, ...);

extern bool ____wrong_branch_error(void);

extern __attribute__((__format__(printf, 3, 4))) void
__dynamic_dev_dbg(struct _ddebug *descriptor, const struct device *dev,
                  const char *fmt, ...);

bool wfx_api_older_than(struct wfx_dev *wdev, int major, int minor);

/* Function defns.  */

static inline unsigned long array_index_mask_nospec(unsigned long index,
                                                    unsigned long size) {
  unsigned long mask;

  asm volatile("cmp %1,%2; sbb %0,%0;"
               : "=r"(mask)
               : "g"(size), "r"(index)
               : "cc");
  return mask;
}

static inline __attribute__((__always_inline__)) bool
arch_static_branch(struct static_key *key, bool branch) {
  asm goto("1:"
           "jmp %l[l_yes] # objtool NOPs this \n\t"
           ".pushsection __jump_table,  \"aw\" \n\t"
           " "
           ".balign 8"
           " "
           "\n\t"
           ".long 1b - . \n\t"
           ".long %l[l_yes] - . \n\t"
           " "
           ".quad"
           " "
           "%c0 + %c1 - .\n\t"
           ".popsection \n\t"
           :
           : "i"(key), "i"(2 | branch)
           :
           : l_yes);
  asm("");

  return false;
l_yes:
  return true;
}

static inline __attribute__((__always_inline__)) bool
arch_static_branch_jump(struct static_key *const key, const bool branch) {
  asm goto("1:"
           "jmp %l[l_yes]\n\t"
           ".pushsection __jump_table,  \"aw\" \n\t"
           " "
           ".balign 8"
           " "
           "\n\t"
           ".long 1b - . \n\t"
           ".long %l[l_yes] - . \n\t"
           " "
           ".quad"
           " "
           "%c0 + %c1 - .\n\t"
           ".popsection \n\t"
           :
           : "i"(key), "i"(branch)
           :
           : l_yes);
  asm("");

  return false;
l_yes:
  return true;
}

static inline struct wfx_vif *wdev_to_wvif(struct wfx_dev *wdev, int vif_id) {
  if (vif_id >=
      (sizeof(wdev->vif) / sizeof((wdev->vif)[0]) + ((int)(sizeof(struct {
         int : (-!!(__builtin_types_compatible_p(typeof((wdev->vif)),
                                                 typeof(&(wdev->vif)[0]))));
       }))))) {
    static struct _ddebug __attribute__((__aligned__(8)))
    __attribute__((__section__("__dyndbg"))) __UNIQUE_ID_ddebug1678 = {
        .modname = "wfx",
        .function = __func__,
        .filename = "drivers/staging/wfx/wfx.h",
        .format = ("requesting non-existent vif: %d\n"),
        .lineno = 97,
        .flags = 0,
        .key.dd_key_false = ((struct static_key_false){
            .key = {.enabled = {0}, {.entries = (void *)0UL}},
        })};
    if (({
          bool branch;
          if (__builtin_types_compatible_p(
                  typeof(*&__UNIQUE_ID_ddebug1678.key.dd_key_false),
                  struct static_key_true))
            branch = arch_static_branch_jump(
                &(&__UNIQUE_ID_ddebug1678.key.dd_key_false)->key, false);
          else if (__builtin_types_compatible_p(
                       typeof(*&__UNIQUE_ID_ddebug1678.key.dd_key_false),
                       struct static_key_false))
            branch = arch_static_branch(
                &(&__UNIQUE_ID_ddebug1678.key.dd_key_false)->key, false);
          else
            branch = ____wrong_branch_error();
          __builtin_expect(!!(branch), 0);
        }))
      __dynamic_dev_dbg(&__UNIQUE_ID_ddebug1678, wdev->dev,
                        "requesting non-existent vif: %d\n", vif_id);
    return NULL;
  }
  typeof(vif_id) _i = (vif_id);
  typeof((sizeof(wdev->vif) / sizeof((wdev->vif)[0]) + ((int)(sizeof(struct {
            int : (-!!(__builtin_types_compatible_p(typeof((wdev->vif)),
                                                    typeof(&(wdev->vif)[0]))));
          }))))) _s =
      ((sizeof(wdev->vif) / sizeof((wdev->vif)[0]) + ((int)(sizeof(struct {
          int : (-!!(__builtin_types_compatible_p(typeof((wdev->vif)),
                                                  typeof(&(wdev->vif)[0]))));
        })))));
  unsigned long _mask = array_index_mask_nospec(_i, _s);
  vif_id = (typeof(_i))(_i & _mask);
  if (!wdev->vif[vif_id]) {
    static struct _ddebug __attribute__((__aligned__(8)))
    __attribute__((__section__("__dyndbg"))) __UNIQUE_ID_ddebug1681 = {
        .modname = "wfx",
        .function = __func__,
        .filename = "drivers/staging/wfx/wfx.h",
        .format = ("requesting non-allocated vif: %d\n"),
        .lineno = 102,
        .flags = 0,
        .key.dd_key_false = ((struct static_key_false){
            .key = {.enabled = {0}, {.entries = (void *)0UL}},
        })};
    if (({
          bool branch;
          if (__builtin_types_compatible_p(
                  typeof(*&__UNIQUE_ID_ddebug1681.key.dd_key_false),
                  struct static_key_true))
            branch = arch_static_branch_jump(
                &(&__UNIQUE_ID_ddebug1681.key.dd_key_false)->key, false);
          else if (__builtin_types_compatible_p(
                       typeof(*&__UNIQUE_ID_ddebug1681.key.dd_key_false),
                       struct static_key_false))
            branch = arch_static_branch(
                &(&__UNIQUE_ID_ddebug1681.key.dd_key_false)->key, false);
          else
            branch = ____wrong_branch_error();
          __builtin_expect(!!(branch), 0);
        }))
      __dynamic_dev_dbg(&__UNIQUE_ID_ddebug1681, wdev->dev,
                        "requesting non-allocated vif: %d\n", vif_id);
    return NULL;
  }
  return (struct wfx_vif *)wdev->vif[vif_id]->drv_priv;
}

int wfx_get_ps_timeout(struct wfx_vif *wvif, bool *enable_ps) {
  struct ieee80211_channel *chan0 = NULL, *chan1 = NULL;
  struct ieee80211_conf *conf = &wvif->wdev->hw->conf;

  if (wdev_to_wvif(wvif->wdev, 0))
    chan0 = wdev_to_wvif(wvif->wdev, 0)->vif->bss_conf.chandef.chan; /* { dg-bogus "dereference of NULL" } */
  if (wdev_to_wvif(wvif->wdev, 1))
    chan1 = wdev_to_wvif(wvif->wdev, 1)->vif->bss_conf.chandef.chan; /* { dg-bogus "dereference of NULL" } */
  if (chan0 && chan1 && chan0->hw_value != chan1->hw_value &&
      wvif->vif->type != NL80211_IFTYPE_AP) {

    if (enable_ps)
      *enable_ps = true;
    if (wvif->wdev->force_ps_timeout > -1)
      return wvif->wdev->force_ps_timeout;
    else if (wfx_api_older_than(wvif->wdev, 3, 2))
      return 0;
    else
      return 30;
  }
  if (enable_ps)
    *enable_ps = wvif->vif->bss_conf.ps;
  if (wvif->wdev->force_ps_timeout > -1)
    return wvif->wdev->force_ps_timeout;
  else if (wvif->vif->bss_conf.assoc && wvif->vif->bss_conf.ps)
    return conf->dynamic_ps_timeout;
  else
    return -1;
}
