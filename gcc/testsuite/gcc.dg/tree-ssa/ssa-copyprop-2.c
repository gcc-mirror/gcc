/* { dg-do compile } */
/* { dg-options "-Og -fdump-tree-optimized" } */

extern long long __sdt_unsp;
void
f(void)
{
  for (;;)
    __asm__ ("%0" :: "i" (((!__extension__ (__builtin_constant_p ((((unsigned long long) (__typeof (__builtin_choose_expr (((__builtin_classify_type (0) + 3) & -4) == 4, (0), 0U))) __sdt_unsp) ) == 0) )) ? 1 : -1) ));
}

/* { dg-final { scan-tree-dump-not "PHI" "optimized" } } */
