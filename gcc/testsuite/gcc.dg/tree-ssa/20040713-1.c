/* { dg-do compile } */
/* { dg-options "-Os" } */

/* Extracted from PR 16443.  Contributed by Volker Reichelt.
   Scanning of __asm__ operands wasn't considering call-clobbered
   variables discovered before the aliasing pass.  This was causing a
   crash in verify_ssa() because 'p' was not being given an SSA_NAME.  */

void foo(char *p)
{
    __asm__ ("" ::: "memory");
}

void bar()
{
    static char *p;
    foo(p);
}
