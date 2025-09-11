/* PR debug/121411.
   Test for compilation of very large struct types.
   The ctt_size for the struct shall encode CTF_LSIZE_SENT to indicate the
   large struct encoding is used.  */

/* { dg-do compile { target { lp64 || llp64 } } } */
/* { dg-options "-O0 -gctf -dA" } */

struct huge
{                               /* bit offset  */
  unsigned char a1[0xffffffff]; /*          0  */
  unsigned char a2[0xffffffff]; /*  7fffffff8  */
  char x;                       /*  ffffffff0  */
  char y;                       /*  ffffffff8  */
  char z;                       /* 1000000000  */
};

struct huge v;

/* Verify struct is encoded with large type encoding format.  */
/* { dg-final { scan-assembler-times "0x1a000005\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "0xffffffff\[\t \]+\[^\n\]*ctt_size" 1 } } */
/* { dg-final { scan-assembler-times "0x2\[\t \]+\[^\n\]*ctt_lsizehi" 1 } } */
/* { dg-final { scan-assembler-times "ctt_lsizelo" 1 } } */

/* Verify member offsets are correct for large offsets.  */
/* { dg-final { scan-assembler-times "0x7\[\t \]+\[^\n\]*ctlm_offsethi" 1 } } */
/* { dg-final { scan-assembler-times "0xf\[\t \]+\[^\n\]*ctlm_offsethi" 2 } } */
/* { dg-final { scan-assembler-times "0x10\[\t \]+\[^\n\]*ctlm_offsethi" 1 } } */

/* { dg-final { scan-assembler-times "0xfffffff8\[\t \]+\[^\n\]*ctlm_offsetlo" 2 } } */
/* { dg-final { scan-assembler-times "0xfffffff0\[\t \]+\[^\n\]*ctlm_offsetlo" 1 } } */
