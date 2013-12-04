/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -g -flto } } } */
/* { dg-extra-ld-options { -r -nostdlib } } */

extern void bar(void);

int main(int argc, char **argv)
{
  int i;

  if (argc == 1) {
    extern void bar ();

    bar();

    {
      extern void bar ();

      asm goto ("" : : : : lab);
lab:
      ;
    }
  }

  {
    extern void bar ();

    int foo(void)
    {
      return argv[0][0];
    }

    i = foo();
  }

  return i;
}
