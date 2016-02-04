/* PR c/68668 */
/* { dg-do compile } */

typedef const int T[];
typedef const int U[1];

int
fn1 (T p)
{
  return p[0];
}

int
fn2 (U p[2])
{
  return p[0][0];
}

int
fn3 (U p[2][3])
{
  return p[0][0][0];
}

int
fn4 (U *p)
{
  return p[0][0];
}

int
fn5 (U (*p)[1])
{
  return (*p)[0][0];
}

int
fn6 (U (*p)[1][2])
{
  return (*p)[0][0][0];
}

int
fn7 (U **p)
{
  return p[0][0][0];
}

int
fn8 (U (**p)[1])
{
  return (*p)[0][0][0];
}
