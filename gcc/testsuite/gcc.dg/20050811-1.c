/* Test whether -dav doesn't crash.  */
/* { dg-do compile } */
/* { dg-options "-O2 -dav" } */

int foo (void)
{
  return 6;
}

int bar (int x)
{
  if (x < 0)
    return foo () + 8;
  else if (x > 0)
    return 2 * foo ();
  else
    return foo ();
}

// { dg-final { cleanup-rtl-dump "*" } }
