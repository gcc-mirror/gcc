/* { dg-options "-O2 -fdump-tree-dse-details -fno-tree-fre -fdump-tree-optimized" } */

/* PR tree-optimization/87901 */

int z[128];
void foo1(void)
{
  int z1;
  int z2[24/sizeof(int)];
  __builtin_memset (&z1, 0, sizeof(int));
  __builtin_memcpy (z, &z1, sizeof(int));
  __builtin_memset (z2, 0, 24);
  __builtin_memcpy (((char*)z)+1, z2, 24);
}

/* we should get:
  MEM[(char * {ref-all})&z] = {};
  __builtin_memset (&MEM <int[128]> [(void *)&z + 1B], 0, 24);
 */

/* { dg-final { scan-tree-dump-not "MEM <unsigned int>" "optimized" } } */
/* { dg-final { scan-tree-dump-not "MEM \\\[" "dse1" } } */
/* { dg-final { scan-tree-dump-times "Trimming statement " 1 "dse1" } } */
/* { dg-final { scan-tree-dump-times "Deleted dead call:" 1 "dse1" } } */

