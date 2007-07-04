/* Check that -mrelax works.  */
/* { dg-do run { target { { sh-*-* sh?-*-* } && { ! { sh*-*-vxworks* && nonpic } } } } } */
/* { dg-options "-O1 -mrelax" } */

extern void abort (void);
extern int qwerty (int);

int
f (int i)
{
  return qwerty (i) + 1;
}

int
qwerty (int i)
{
  switch (i)
    {
    case 1:
      return 'q';
    case 2:
      return 'w';
    case 3:
      return 'e';
    case 4:
      return 'r';
    case 5:
      return 't';
    case 6:
      return 'y';
    }
}

int
main ()
{
  if (f (1) != 'q' + 1 || f (2) != 'w' + 1 || f (3) != 'e' + 1
      || f(4) != 'r' + 1 || f (5) != 't' + 1 || f (6) != 'y' + 1)
    abort ();
  return 0;
}
