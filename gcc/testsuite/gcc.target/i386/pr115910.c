/* PR target/115910 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler-times {\timulq\t} 2 } } */
/* { dg-final { scan-assembler-times {\tshrq\t\$33,} 2 } } */
/* { dg-final { scan-assembler-not {\tsarl\t} } } */

int
foo (int x)
{
  if (x < 0) 
    __builtin_unreachable ();
  return x / 3U;
}

int
bar (int x)
{
  return x / 3U;
}
