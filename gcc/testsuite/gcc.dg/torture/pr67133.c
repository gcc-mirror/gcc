/* { dg-do compile } */
/* { dg-additional-options "-fisolate-erroneous-paths-attribute" } */

int printf (const char *, ...);
int foo (int);

int a, *b, c;

static int
fn1 ()
{ 
  if (a)
    return (a = 0);
  for (; a; )
    a = 0;
  return 0;
}

static int
fn2 (int p)
{ 
  fn1 ();
  c = 0;
  if (p)
    printf ("%d", 0);
  foo (b != &p);
  return 0;
}

void
fn3 ()
{ 
  fn2 (0);
}
