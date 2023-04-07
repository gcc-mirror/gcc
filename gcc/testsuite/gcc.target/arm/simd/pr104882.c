/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

int i;
char src[1072];
char dst[72];
int main() {
  for (i = 0; i < 128; i++)
    src[i] = i;
  __builtin_memcpy(dst, src, 7);
  for (i = 0; i < 7; i++)
    if (dst[i] != i)
      __builtin_abort();
}
