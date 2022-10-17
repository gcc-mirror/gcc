/* Reduced from infoleak false positive seen on Linux kernel with
   net/ethtool/ioctl.c  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

typedef signed char __s8;
typedef unsigned char __u8;
typedef unsigned int __u32;
typedef __s8 s8;
typedef __u32 u32;
enum { false = 0, true = 1 };
typedef unsigned long __kernel_ulong_t;
typedef __kernel_ulong_t __kernel_size_t;
typedef _Bool bool;
typedef __kernel_size_t size_t;

void *memset(void *s, int c, size_t n);

extern bool
check_copy_size(const void *addr, size_t bytes, bool is_source);
extern unsigned long
_copy_from_user(void *, const void *, unsigned long);
extern unsigned long
_copy_to_user(void *, const void *, unsigned long);

static inline
__attribute__((__always_inline__)) unsigned long
copy_from_user(void *to, const void *from, unsigned long n) {
  if (__builtin_expect(!!(check_copy_size(to, n, false)), 1))
    n = _copy_from_user(to, from, n);
  return n;
}
static inline
__attribute__((__always_inline__)) unsigned long
copy_to_user(void *to, const void *from, unsigned long n) {
  if (__builtin_expect(!!(check_copy_size(from, n, true)), 1))
    n = _copy_to_user(to, from, n);
  return n;
}
enum ethtool_link_mode_bit_indices {
  __ETHTOOL_LINK_MODE_MASK_NBITS = 92
};
struct ethtool_link_settings {
  __u32 cmd;
  /* [...snip...] */
  __s8 link_mode_masks_nwords;
  /* [...snip...] */
};

struct ethtool_link_ksettings {
  struct ethtool_link_settings base;
  u32 lanes;
};

int ethtool_get_link_ksettings(void *useraddr) {
  int err = 0;
  struct ethtool_link_ksettings link_ksettings;

  if (copy_from_user(&link_ksettings.base, useraddr,
                     sizeof(link_ksettings.base)))
    return -14;

  if ((((__ETHTOOL_LINK_MODE_MASK_NBITS) + (32) - 1) / (32)) !=
      link_ksettings.base.link_mode_masks_nwords) {

    memset(&link_ksettings, 0, sizeof(link_ksettings));
    link_ksettings.base.cmd = 0x0000004c;

    link_ksettings.base.link_mode_masks_nwords =
        -((s8)(((__ETHTOOL_LINK_MODE_MASK_NBITS) + (32) - 1) / (32)));

    if (copy_to_user(useraddr, &link_ksettings.base,
                     sizeof(link_ksettings.base)))
      return -14;

    return 0;
  }

  return 0;
}
