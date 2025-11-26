/* PR middle-end/122835 */
/* { dg-do run { target i?86-*-* x86_64-*-* aarch64-*-* arm*-*-* powerpc*-*-* s390*-*-* } } */

#if defined(__x86_64__) || defined(__i386__)
#define JMP "jmp"
#elif defined(__aarch64__) || defined(__arm__) || defined(__powerpc__)
#define JMP "b"
#elif defined(__s390__)
#define JMP "j"
#endif

int cnt;

static void
my_cleanup (int *p)
{
  ++cnt;
}

__attribute__((noipa)) static void
my_abort (void)
{
  __builtin_abort ();
}

int
main ()
{
  {
    int x __attribute__((cleanup (my_cleanup))) = 0;

    asm goto (JMP "\t%l0" :::: l1);

    my_abort ();
  }

l1:
  if (cnt != 1)
    __builtin_abort ();

  {
    int x __attribute__((cleanup (my_cleanup))) = 0;

    {
      int y __attribute__((cleanup (my_cleanup))) = 0;

      asm goto (JMP "\t%l1" :::: l2, l3);

      my_abort ();
    }
l2:
    __builtin_abort ();
  }
l3:
  if (cnt != 3)
    __builtin_abort ();

  {
    int x __attribute__((cleanup (my_cleanup))) = 0;

    {
      int y __attribute__((cleanup (my_cleanup))) = 0;

      asm goto (JMP "\t%l0" :::: l4, l5);

      my_abort ();
    }
l4:
    if (cnt != 4)
      __builtin_abort ();
  }
  if (0)
    {
l5:
      __builtin_abort ();
    }
  if (cnt != 5)
    __builtin_abort ();
}
