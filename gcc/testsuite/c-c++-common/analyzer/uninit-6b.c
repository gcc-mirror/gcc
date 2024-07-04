/* Reduced from uninit false positive seen on Linux kernel with
   net/ethtool/ioctl.c  */

typedef signed char s8;
typedef unsigned int u32;
typedef __SIZE_TYPE__ size_t;

void *memset(void *s, int c, size_t n);

struct ethtool_link_settings {
  u32 cmd;
  s8 link_mode_masks_nwords;
};

struct ethtool_link_ksettings {
  u32 lanes;
  struct ethtool_link_settings base;
};

struct ethtool_link_settings
ethtool_get_link_ksettings(void) {
  struct ethtool_link_ksettings link_ksettings;

  memset(&link_ksettings, 0, sizeof(link_ksettings));
  link_ksettings.base.cmd = 0x0000004c;
  link_ksettings.base.link_mode_masks_nwords = -3;

  return link_ksettings.base;
}
