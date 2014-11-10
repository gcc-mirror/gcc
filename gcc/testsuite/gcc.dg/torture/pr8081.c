/* { dg-do run } */
/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */

extern void abort (void);
int
main (int argc, char **argv)
{
  int size = 10;
  typedef struct
    {
      char val[size];
    }
  block;
  block a, b;
  block __attribute__((noinline))
  retframe_block ()
    {
      return *(block *) &b;
    }
  b.val[0] = 1;
  b.val[9] = 2;
  a=retframe_block ();
  if (a.val[0] != 1
      || a.val[9] != 2)
    abort ();
  return 0;
}
