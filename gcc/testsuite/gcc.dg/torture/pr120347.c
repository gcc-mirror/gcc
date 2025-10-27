/* { dg-do assemble } */
/* { dg-additional-options "-march=armv7-a -mthumb" { target { arm_arch_v7a_ok && arm_thumb2_ok } } } */

void *end;
void **start;
void main(void)
{
  for (; end; start++) {
    if (*start)
      return;
    *start = start;
  }
}
