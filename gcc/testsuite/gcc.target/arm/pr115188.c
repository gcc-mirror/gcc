/* { dg-do assemble } */
/* { dg-require-effective-target arm_arch_v6m_ok }
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v6m } */

void init (int *p, int n)
{
  for (int i = 0; i < n; i++)
    __atomic_store_4 (p + i, 0, __ATOMIC_RELAXED);
}
