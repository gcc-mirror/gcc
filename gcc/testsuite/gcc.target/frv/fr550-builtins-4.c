/* Test the __M{,D}{ADD,SUB}ACC functions.  */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int
main ()
{
  __MWTACC (6, 10);
  __MWTACC (7, 25);
  __MADDACCS (5, 6);
  if (__MRDACC (5) != 35)
    abort ();
  __MSUBACCS (4, 6);
  if (__MRDACC (4) != -15)
    abort ();
  __MASACCS (4, 6);
  if (__MRDACC (4) != 35 || __MRDACC (5) != -15)
    abort ();

  __MWTACC (0, 100);
  __MWTACC (1, 150);
  __MWTACC (2, 1000);
  __MWTACC (3, 1500);
  __MDADDACCS (2, 0);
  if (__MRDACC (2) != 250 || __MRDACC (3) != 2500)
    abort ();

  __MWTACC (0, 100);
  __MWTACC (1, 150);
  __MWTACC (2, 1000);
  __MWTACC (3, 1500);
  __MDSUBACCS (2, 0);
  if (__MRDACC (2) != -50 || __MRDACC (3) != -500)
    abort ();

  __MWTACC (0, 100);
  __MWTACC (1, 150);
  __MWTACC (2, 1000);
  __MWTACC (3, 1500);
  __MDASACCS (0, 0);
  if (__MRDACC (0) != 250 || __MRDACC (1) != -50)
    abort ();
  if (__MRDACC (2) != 2500 || __MRDACC (3) != -500)
    abort ();

  exit (0);
}
