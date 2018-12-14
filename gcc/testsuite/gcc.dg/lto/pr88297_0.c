/* { dg-require-effective-target lto } */
/* { dg-lto-options { { -flto -O3 -fipa-cp -fipa-cp-clone } } } */
/* { dg-lto-do run } */

/* In order to trigger IPA-CP cloning we have to:

  1. Put the calls in main into a loop; otherwise everything is
  cold and we would not clone.

  2. Make different foos and bars actually semantically different;
  otherwise IPA-ICF unified them (as it should).

*/

volatile int g;

void __attribute__ ((noipa))
use (int v)
{
  g = v;
}

static int __attribute__ ((noinline))
foo (int arg)
{
  return 7 * arg;
}

static int __attribute__ ((noinline))
bar (int arg)
{
  return arg * arg;
}

extern int __attribute__ ((noinline))
entry2 (void);

int  __attribute__ ((noipa))
get_opaque_number (void)
{
  return 1;
}

int main (void)
{
  int i;
  for (i = 0; i < get_opaque_number (); i++)
    {
      use (bar (3));
      use (bar (4));
      use (foo (5));
      use (foo (6));

      entry2 ();
    }
  return 0;
}
