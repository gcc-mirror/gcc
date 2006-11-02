/* PR other/29639 */
/* AIX gld supports garbage collection. But AIX gcc does not support 
   -ffunction-sections or -fdata-sections.  */
/* { dg-do run { xfail rs6000-*-aix* powerpc*-*-aix* } } */
/* { dg-require-gc-sections "" } */
/* { dg-options "-ffunction-sections -Wl,--gc-sections" } */

extern "C" void abort (void);

int g = 0;

void raise_exception()
{
  throw 1;
}

void used()
{
  try {
    raise_exception ();
  }
  catch (int) {
    g = 1;
  }
}

void unused()
{
  try {
    raise_exception ();
  }
  catch (int) {
    g = 1;
  }
}

int main()
{
  used ();

  if (g != 1)
    abort ();

  return 0;
}
