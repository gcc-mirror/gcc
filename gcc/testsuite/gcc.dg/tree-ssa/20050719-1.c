/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void) __attribute__ ((__nothrow__)) __attribute__
((__noreturn__));
extern void exit (int __status) __attribute__ ((__nothrow__))
__attribute__ ((__noreturn__));

struct bootLoader {
  int x;
};

void
zap(struct bootLoader *bootLoader)
{
  /* The expression on the RHS of the assignment is *not* a
     dereference of pointer 'bootLoader'.  It is merely used as an
     offset calculation.  VRP was erroneously removing the if()
     because it thought that 'bootLoader' was always dereferenced.  */
  int *boot = &bootLoader->x;

  if (bootLoader)
    {
      useboot (boot);
    }
}

int
useboot (void *boot)
{
  abort ();
}

main()
{
  zap (0);
  return 0;
}
