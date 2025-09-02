/* PR debug/121411
   CTF generation for array which cannot be encoded in CTF.

   CTF encoding uses a uint32 for number of elements in an array which
   means there is a hard upper limit on sizes of arrays which can be
   represented.  Arrays with too many elements are encoded with
   CTF_K_UNKNOWN to indicate that they cannot be represented.  */

/* { dg-do compile { target { lp64 || llp64 } } } */
/* { dg-options "-O0 -gctf -dA" } */

int   rep[0xffffffff];
int unrep[0x100000000];

/* One dimension can be represented, other cannot.
   Result is a (representable) array with unknown element type.  */
int unrepdim [0xab][0x100000007];

/* Two CTF_K_ARRAY, one (shared) CTF_K_UNKNOWN.  */
/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 2 } } */
/* { dg-final { scan-assembler-times "0x2000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]+0xffffffff\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]+0xab\[\t \]+\[^\n\]*cta_nelems" 1 } } */
