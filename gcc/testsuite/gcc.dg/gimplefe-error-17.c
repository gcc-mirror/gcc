/* { dg-do compile } */
/* { dg-options "-fgimple -Wno-psabi" } */

typedef int v4si __attribute__ ((vector_size (16)));

int __GIMPLE
f1 (int x)
{
  int r;
  r = .CLZ (x); /* { dg-error "'.CLZ' call with an argument that is not unsigned integer" } */
  return r;
}

v4si __GIMPLE
f2 (v4si x)
{
  v4si r;
  r = .CTZ (x); /* { dg-error "'.CTZ' call with an argument that is not unsigned integer" } */
  return r;
}

int __GIMPLE
f3 (unsigned int x)
{
  int r;
  r = .CLZ (x, 32, 0); /* { dg-error "'.CLZ' call with too many arguments" } */
  return r;
}

int __GIMPLE
f4 (unsigned int x, int y)
{
  int r;
  r = .CTZ (x, y); /* { dg-error "'.CTZ' call with a value at zero that is not constant" } */
  return r;
}

int __GIMPLE
f5 ()
{
  int r;
  r = .CLZ (); /* { dg-error "'.CLZ' call with too few arguments" } */
  return r;
}

double __GIMPLE
f6 (double x)
{
  double r;
  r = .FMA (x, x); /* { dg-error "'.FMA' call with too few arguments" } */
  return r;
}

double __GIMPLE
f7 (double x)
{
  double r;
  r = .SQRT (x, x); /* { dg-error "'.SQRT' call with too many arguments" } */
  return r;
}

void __GIMPLE
f8 (int x)
{
  .CTZ (x); /* { dg-error "'.CTZ' call with an argument that is not unsigned integer" } */
  return;
}

int __GIMPLE
f9 (unsigned int x)
{
  int r;
  r = .CLZ (y); /* { dg-error "'y' undeclared" } */
  /* { dg-message "each undeclared identifier" "" { target *-*-* } .-1 } */
  .CTZ (y);
  return r;
}
