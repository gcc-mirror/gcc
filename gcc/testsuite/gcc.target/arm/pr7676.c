/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a -mfloat-abi=soft -mthumb" }  */
/* { dg-skip-if "Do not combine float-abi= hard | soft | softfp" {*-*-*} {"-mfloat-abi=hard" -mfloat-abi=softfp } {""} } */
int a;

void fn2 (long long);

void
fn1 ()
{
  long long b;

  for (;;)
    {
      b = ((a == (int)&fn1) <= 0 ^ 18446744073709551608ULL)
	- 18446744073709551615ULL;

      fn2 (b);
    }
}
