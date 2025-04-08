/* { dg-options "-O2 -fdump-tree-dse-details -fno-tree-fre -fdump-tree-optimized" } */

/* PR tree-optimization/87901 */

char z[32];
void foo1(void)
{
  char z1[17];
  char z2[24];
  __builtin_memset (z1, 0, 17);
  __builtin_memcpy (z, z1, 17);
  __builtin_memset (z2, 0, 24);
  __builtin_memcpy (z+8, z2, 24);
}

/* we should get:
  MEM <unsigned char[8]> [(char * {ref-all})&z] = {};
  MEM <unsigned char[24]> [(char * {ref-all})&z + 8B] = {};
  after DSE; trimming the first memset to z (which was memcpy) to 8 bytes
  from the original 17.
  and not have a [17] in the IR after DSE.
  The two memset to z1/z2 will also be removed.
 */
/* { dg-final { scan-tree-dump-not "\\\[17\\\]" "optimized" } } */
/* { dg-final { scan-tree-dump "\\\[8\\\]" "dse1" } } */

/* { dg-final { scan-tree-dump-times "Trimming statement " 1 "dse1" } } */
/* { dg-final { scan-tree-dump-times "Deleted dead call:" 2 "dse1" } } */


