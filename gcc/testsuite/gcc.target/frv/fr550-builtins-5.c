/* Test that __MADDACC only changes the registers it's supposed to.  */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int
main ()
{
  __MWTACC (0, 1);
  __MWTACC (1, 1);
  __MWTACC (2, 1);
  __MWTACC (3, 1);
  __MWTACC (4, 1);
  __MWTACC (5, 1);
  __MWTACC (6, 1);
  __MWTACC (7, 1);
  __MADDACCS (0, 2);
  __MADDACCS (4, 6);
  if ((__MRDACC (0) - 2)
      | (__MRDACC (1) - 1)
      | (__MRDACC (4) - 2)
      | (__MRDACC (5) - 1))
    abort ();
  exit (0);
}
