/* Check that ACCs and ACCGs are treated as global variables even if
   media.h isn't included.  */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

void
set ()
{
#define SET(X) __MWTACC (X, (X) | 0x100), __MWTACCG (X, X)
  SET (0);
  SET (1);
  SET (2);
  SET (3);
#if defined (__CPU_FR450__)
  SET (8);
  SET (9);
  SET (10);
  SET (11);
#elif __FRV_ACC__ > 4
  SET (4);
  SET (5);
  SET (6);
  SET (7);
#endif
#undef SET
}

void
check ()
{
  int diff1, diff2;

  diff1 = diff2 = 0;

#define CHECK(X) \
	(diff1 |= (__MRDACC (X) ^ (X | 0x100)), \
	 diff2 |= (__MRDACCG (X) ^ X))
  CHECK (0);
  CHECK (1);
  CHECK (2);
  CHECK (3);
#if defined (__CPU_FR450__)
  CHECK (8);
  CHECK (9);
  CHECK (10);
  CHECK (11);
#elif __FRV_ACC__ > 4
  CHECK (4);
  CHECK (5);
  CHECK (6);
  CHECK (7);
#endif
#undef CHECK
  if ((diff1 | diff2) != 0)
    abort ();
}

int
main ()
{
  set ();
  check ();
  exit (0);
}
