/* PR target/35664 */
/* Tetstcase by Mike Stein <mstein.lists@googlemail.com> */

/* { dg-do compile } */
/* { dg-options "-O2 -mno-fpu" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
struct pt_regs {
};
static inline __attribute__((always_inline)) struct task_struct *__get_current(void)
{
}
static inline __attribute__((always_inline)) u32 flip_dword (u32 l)
{
 return ((l&0xff)<<24) | (((l>>8)&0xff)<<16) | (((l>>16)&0xff)<<8)| ((l>>24)&0xff);
}
static inline __attribute__((always_inline)) u32 __readl(const volatile void *addr)
{
 return flip_dword(*( volatile u32 *)addr);
}
enum e1e_registers {
 E1000_PRC64 = 0x0405C,
 E1000_PRC127 = 0x04060,
 E1000_PRC255 = 0x04064,
 E1000_PTC511 = 0x040E4,
 E1000_PTC1023 = 0x040E8,
 E1000_PTC1522 = 0x040EC,
 E1000_MPTC = 0x040F0,
};
enum e1000_media_type {
 e1000_media_type_copper = 1,
};
struct e1000_rx_desc {
 struct {
 } wb;
};
struct e1000_hw_stats {
 u64 prc64;
 u64 prc127;
 u64 prc255;
 u64 ptc511;
 u64 ptc1023;
 u64 ptc1522;
 u64 mptc;
};
struct e1000_shadow_ram {
 u16 value;
};
struct e1000_dev_spec_ich8lan {
 struct e1000_shadow_ram shadow_ram[2048];
};
struct e1000_hw {
 u8 *hw_addr;
 union {
  struct e1000_dev_spec_ich8lan ich8lan;
 } dev_spec;
 enum e1000_media_type media_type;
};
struct e1000_adapter {
 u16 link_speed;
 struct e1000_hw hw;
 struct e1000_hw_stats stats;
 unsigned int flags;
};
static inline __attribute__((always_inline)) u32 __er32(struct e1000_hw *hw, unsigned long reg)
{
 return __readl(hw->hw_addr + reg);
}
int e1e_rphy(struct e1000_hw *, int, u16 *);
void e1000e_update_stats(struct e1000_adapter *adapter)
{
 struct e1000_hw *hw = &adapter->hw;
 u16 phy_tmp;
 if (adapter->flags & (1 << 10)) {
  adapter->stats.prc64 += __er32(hw, E1000_PRC64);
  adapter->stats.prc127 += __er32(hw, E1000_PRC127);
  adapter->stats.prc255 += __er32(hw, E1000_PRC255);
  adapter->stats.ptc511 += __er32(hw, E1000_PTC511);
  adapter->stats.ptc1023 += __er32(hw, E1000_PTC1023);
  adapter->stats.ptc1522 += __er32(hw, E1000_PTC1522);
 }
 adapter->stats.mptc += __er32(hw, E1000_MPTC);
 if (hw->media_type == e1000_media_type_copper) {
  if ((adapter->link_speed == 1000) &&
     (!e1e_rphy(hw, 0x0A, &phy_tmp))) {
  }
 }
}
