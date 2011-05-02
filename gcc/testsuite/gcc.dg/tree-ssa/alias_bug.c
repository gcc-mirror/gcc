/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-optimized" } */

typedef unsigned u32;
typedef unsigned short u16;
typedef unsigned char u8;
struct biosregs {
 union {
  struct {
   u32 edi;
   u32 esi;
   u32 ebp;
   u32 _esp;
   u32 ebx;
   u32 edx;
   u32 ecx;
   u32 eax;
   u32 _fsgs;
   u32 _dses;
   u32 eflags;
  };
  struct {
   u16 di, hdi;
   u16 si, hsi;
   u16 bp, hbp;
   u16 _sp, _hsp;
   u16 bx, hbx;
   u16 dx, hdx;
   u16 cx, hcx;
   u16 ax, hax;
   u16 gs, fs;
   u16 es, ds;
   u16 flags, hflags;
  };
  struct {
   u8 dil, dih, edi2, edi3;
   u8 sil, sih, esi2, esi3;
   u8 bpl, bph, ebp2, ebp3;
   u8 _spl, _sph, _esp2, _esp3;
   u8 bl, bh, ebx2, ebx3;
   u8 dl, dh, edx2, edx3;
   u8 cl, ch, ecx2, ecx3;
   u8 al, ah, eax2, eax3;
  };
 };
};
void initregs(struct biosregs *regs);
void intcall(u8 int_no, const struct biosregs *ireg, struct biosregs *oreg);
static u32 *const gp = (u32*) 0x32;
void keyboard_set_repeat(void)
{
 struct biosregs ireg;
 *gp = 10;
 initregs(&ireg);
 ireg.ax = 0x0305;
 intcall(0x16, &ireg, ((void *)0));
}

/* { dg-final { scan-tree-dump-times "ireg.*ax" 1 "optimized"} } */
 
/* { dg-final { cleanup-tree-dump "optimized" } } */
