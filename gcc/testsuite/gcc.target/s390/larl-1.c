/* Check if load-address-relative instructions are created */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z10 -mzarch -fno-section-anchors" } */

/* An explicitely misaligned symbol.  This symbol is NOT aligned as
   mandated by our ABI.  However, the back-end needs to handle that in
   order to make things like __attribute__((packed)) work.  The symbol
   address is expected to be loaded from literal pool.  */
/* { dg-final { scan-assembler "lgrl\t%r2," { target { lp64 } } } } */
/* { dg-final { scan-assembler "lrl\t%r2," { target { ! lp64 } } } } */
extern char align1 __attribute__((aligned(1)));

/* { dg-final { scan-assembler "larl\t%r2,align2" } } */
extern char align2 __attribute__((aligned(2)));

/* { dg-final { scan-assembler "larl\t%r2,align4" } } */
extern char align4 __attribute__((aligned(4)));

/* An external char symbol without explicit alignment has a DECL_ALIGN
   of just 8. In contrast to local definitions DATA_ABI_ALIGNMENT is
   NOT applied to DECL_ALIGN in that case.  Make sure the backend
   still assumes this symbol to be aligned according to ABI
   requirements.  */
/* { dg-final { scan-assembler "larl\t%r2,align_default" } } */
extern char align_default;

char * foo1 () { return &align1; }
char * foo2 () { return &align2; }
char * foo3 () { return &align4; }
char * foo4 () { return &align_default; }

