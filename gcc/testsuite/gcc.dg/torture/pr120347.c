/* { dg-do assemble } */
/* { dg-additional-options "-mcpu=unset -march=armv7-a+fp -mthumb" { target { arm_arch_v7a_ok && arm_thumb2_ok } } } */

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
