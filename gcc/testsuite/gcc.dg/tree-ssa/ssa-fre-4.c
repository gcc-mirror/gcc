/* If the target returns false for TARGET_PROMOTE_PROTOTYPES, then there
   will be no casts for FRE to eliminate and the test will fail.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* hppa*-*-* mips*-*-* m68k*-*-* } } */
/* { dg-options "-O -fno-tree-forwprop -fdump-tree-fre1-details" } */

/* From PR21608.  */

static inline char wrap(char f) { return f; }
char bar(char f)
{
        return wrap(f);
}

/* { dg-final { scan-tree-dump "Replaced \\\(char\\\) .*with " "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
