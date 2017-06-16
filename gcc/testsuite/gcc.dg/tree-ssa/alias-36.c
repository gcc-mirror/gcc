/* PR tree-optimization/80934 - bzero should be assumed not to escape
   pointer argument
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-alias" } */

void foobar (void);

void f (void);

void g (void)
{
  char d[32];
  __builtin_memset (d, 0, sizeof d);
  f ();
  if (*d != 0)
    foobar ();
}

void h (void)
{
  char d[32];
  __builtin_bzero (d, sizeof d);
  f ();
  if (*d != 0)
    foobar ();
}

/* { dg-final { scan-tree-dump-not "memset|foobar|bzero" "alias" } } */
