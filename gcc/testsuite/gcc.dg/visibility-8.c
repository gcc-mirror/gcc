/* Test hidden visibility on built-in functions (for libc).  PR 13856.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*__GI_fputs_unlocked" } } */

int fputs_unlocked (const char *restrict, int *restrict)
   __asm__ ("__GI_fputs_unlocked")
   __attribute__ ((visibility ("hidden")));

int
fputs_unlocked (str, fp)
     const char *str;
     int *fp;
{
}
