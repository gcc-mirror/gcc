/* PR ipa/78791 */

int val;

int *ptr = &val;
float *ptr2 = (float *) &val;

static void
__attribute__((always_inline, optimize ("-fno-strict-aliasing")))
typepun ()
{
  *ptr2=0;
}

int
main(void)
{
  *ptr=1;
  typepun ();
  if (*ptr)
    __builtin_abort ();
}
