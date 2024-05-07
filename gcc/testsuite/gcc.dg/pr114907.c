/* PR middle-end/114907 */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16_runtime } */
/* { dg-add-options bfloat16 } */
/* { dg-require-effective-target bfloat16_runtime } */

__attribute__((noipa)) _Float16
foo (__bf16 x)
{
  return (_Float16) x;
}

__attribute__((noipa)) __bf16
bar (_Float16 x)
{
  return (__bf16) x;
}

int
main ()
{
  if (foo (11.125bf16) != 11.125f16
      || bar (11.125f16) != 11.125bf16)
    __builtin_abort ();
}
