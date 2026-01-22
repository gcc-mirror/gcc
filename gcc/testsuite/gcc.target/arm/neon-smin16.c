/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-additional-options "-std=c23 -Ofast" } */
void foo(void);
extern _Float16 main_in[];
void bar() {
  _Float16 out = 0.0;
  for (int i = 0; i < 100; i++)
    if (out > main_in[i])
      out = main_in[i];
  if (out)
    foo();
}
