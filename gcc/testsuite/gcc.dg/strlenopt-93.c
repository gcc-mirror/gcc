/* Verify that strlen doesn't (inadvertently) use the size of an array
   of char pointers to put an upper bound on the length of the strings
   they point to.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

void eaa_test (void)
{
  extern char eaa[4][4];

  char (*p)[4] = eaa;

  if (!*p)
    return;

  /* The longest string stored in EAA is 15 characters.  */
  if (__builtin_strlen (*p) > 14)
    {
      extern void eaa_ok (void);
      eaa_ok ();
    }

  if (__builtin_strlen (*p) > 15)
    {
      extern void eaa_fail (void);
      eaa_fail ();
    }
}

/* { dg-final { scan-tree-dump-times "eaa_ok" 1 "optimized" } }
   { dg-final { scan-tree-dump-not "eaa_fail" "optimized" } } */


void epa_test (void)
{
  extern char* epa[4];
  char **p = epa;

  if (*p && __builtin_strlen (*p) > 123)
    {
      extern void epa_ok (void);
      epa_ok ();
    }
}

/* { dg-final { scan-tree-dump-times "epa_ok" 1 "optimized" } } */


static char* spa[4];

void spa_test (void)
{
  char **p = spa;

  if (*p && __builtin_strlen (*p) > 123)
    {
      extern void spa_ok ();
      spa_ok ();
    }
}

/* { dg-final { scan-tree-dump-times "spa_ok" 1 "optimized" } } */


void sink (void*, ...);

void init (void)
{
  /* Make believe even the static array SA may be non-zero.  */
  sink (spa);
}
