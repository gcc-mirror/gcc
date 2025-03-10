/* PR libgcc/119151 */
/* Should be run just on targets which don't have _Unwind_Find_FDE in libc.so.  */
/* { dg-do run { target { { x86_64-*-linux* aarch64*-*-linux* powerpc64*-*-linux* riscv*-*-linux* } && lp64 } } } */
/* { dg-options "-O2" } */

struct object
{
  void *pc_begin, *tbase, *dbase, *single;
  __SIZE_TYPE__ i;
  void *fde_end, *next;
};
struct dwarf_eh_bases
{
  void *tbase, *dbase, *func;
};
extern void __register_frame_info (const void *, struct object *);
extern void *__deregister_frame_info (const void *);
extern const void *_Unwind_Find_FDE (void *, struct dwarf_eh_bases *);
#define DW_EH_PE_sdata8	0x0c
#define DW_EH_PE_pcrel	0x10
#define DW_CFA_def_cfa	0x0c
#define DW_CFA_offset	0x80

struct __attribute__((aligned (8))) eh_frame_cie {
  unsigned len;
  unsigned tag;
  unsigned char version;
  unsigned char augmentation[3];
  unsigned char code_align_factor;
  unsigned char data_align_factor;
  unsigned char ra_column;
  unsigned char augmentation_size;
  unsigned char encoding;
  unsigned char def_cfa;
  unsigned char def_cfa_op1, def_cfa_op2;
  unsigned char offset;
  unsigned char offset_op;
};
struct __attribute__((aligned (8))) eh_frame_fde {
  unsigned len;
  unsigned cie_offset;
  unsigned long long begin, size;
  unsigned char augmentation;
};
struct eh_frame_cie_fde {
  struct eh_frame_cie cie;
  struct eh_frame_fde fde;
  unsigned int zero;
  struct object obj;
} eh_frame[256];
unsigned ehidx;
unsigned char code[0x800] __attribute__((aligned (8)));

void *
register_range (void *addr, unsigned size)
{
  /* Fills in empty-ish CIE and FDE with pcrel sdata8 encoding so that
     we don't need to worry about lp64 large code models.
     We don't actually execute anything in code and only _Unwind_Find_FDE,
     don't actually try to unwind anything.  */
  eh_frame[ehidx].cie.len
    = (unsigned) ((char *) &eh_frame[ehidx].fde
		  - (char *) &eh_frame[ehidx].cie.tag);
  eh_frame[ehidx].cie.tag = 0;
  eh_frame[ehidx].cie.version = 3;
  __builtin_memcpy (eh_frame[ehidx].cie.augmentation, "zR", 3);
  eh_frame[ehidx].cie.code_align_factor = 1;
  eh_frame[ehidx].cie.data_align_factor = 0x78; /* sleb128 -8 */
  eh_frame[ehidx].cie.ra_column = 0x10;
  eh_frame[ehidx].cie.augmentation_size = 1;
  eh_frame[ehidx].cie.encoding = DW_EH_PE_pcrel | DW_EH_PE_sdata8;
  eh_frame[ehidx].cie.def_cfa = DW_CFA_def_cfa;
  eh_frame[ehidx].cie.def_cfa_op1 = 7;
  eh_frame[ehidx].cie.def_cfa_op2 = 8;
  eh_frame[ehidx].cie.offset = DW_CFA_offset + 0x10;
  eh_frame[ehidx].cie.offset_op = 1;
  eh_frame[ehidx].fde.len
    = (unsigned) ((char *) &eh_frame[ehidx].zero
		  - (char *) &eh_frame[ehidx].fde.cie_offset);
  eh_frame[ehidx].fde.cie_offset
    = (unsigned) ((char *) &eh_frame[ehidx].fde.cie_offset
		  - (char *) &eh_frame[ehidx].cie);
  eh_frame[ehidx].fde.begin
    = (__INTPTR_TYPE__) ((__UINTPTR_TYPE__) addr
			 - (__UINTPTR_TYPE__) &eh_frame[ehidx].fde.begin);
  eh_frame[ehidx].fde.size = size;
  eh_frame[ehidx].fde.augmentation = 0;
  eh_frame[ehidx].zero = 0;
  __register_frame_info (&eh_frame[ehidx].cie, &eh_frame[ehidx].obj);
  ++ehidx;
  return &eh_frame[ehidx - 1].cie;
}

void
unregister (void *eh_frame)
{
  __deregister_frame_info (eh_frame);
}

int
main ()
{
  for (int i = 0; i < 0x50; i += 0x10)
    register_range (&code[i], 10);
  void *p = register_range (&code[0x50], 10);
  for (int i = 0x60; i < 0xb0; i += 0x10)
    register_range (&code[i], 10);
  unregister (p);
  register_range (&code[0x4c], 8);
  struct dwarf_eh_bases bases;
  const void *q = _Unwind_Find_FDE (&code[0x4c], &bases);
  const void *r = _Unwind_Find_FDE (&code[0x51], &bases);
  if (!q || q != r)
    __builtin_abort ();
  for (int i = 0; i <= 0xa0; i += 0x10)
    if (i != 0x50)
      {
        q = _Unwind_Find_FDE (&code[i], &bases);
        r = _Unwind_Find_FDE (&code[i + 9], &bases);
        if (!q || q != r)
	  __builtin_abort ();
      }
  for (int i = 0xb0; i < 0x240; i += 0x10)
    register_range (&code[i], 10);
  p = register_range (&code[0x240], 10);
  for (int i = 0x250; i < 0x470; i += 0x10)
    register_range (&code[i], 10);
  void *s = register_range (&code[0x470], 10);
  for (int i = 0x480; i < 0x700; i += 0x10)
    register_range (&code[i], 10);
  unregister (p);
  register_range (&code[0x23c], 16);
  q = _Unwind_Find_FDE (&code[0x23d], &bases);
  r = _Unwind_Find_FDE (&code[0x24b], &bases);
  if (!q || q != r)
    __builtin_abort ();
  unregister (s);
  register_range (&code[0x46c], 16);
  q = _Unwind_Find_FDE (&code[0x46d], &bases);
  r = _Unwind_Find_FDE (&code[0x47b], &bases);
  if (!q || q != r)
    __builtin_abort ();
  for (int i = 0; i < 0x700; i += 0x10)
    if (i != 0x50 && i != 0x240 && i != 0x470)
      {
        q = _Unwind_Find_FDE (&code[i], &bases);
        r = _Unwind_Find_FDE (&code[i + 9], &bases);
        if (!q || q != r)
	  __builtin_abort ();
      }
}
