/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */
/* { dg-additional-options "-std=gnu11" { target c } } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

signed char
fn1 (signed char x, unsigned long y)
{
  return x << y;
}

short int
fn2 (short int x, unsigned long y)
{
  return x << y;
}

int
fn3 (int x, unsigned long y)
{
  return x << y;
}

long int
fn4 (long int x, unsigned long y)
{
  return x << y;
}

long long int
fn5 (long long int x, unsigned long y)
{
  return x << y;
}

signed char
fn6 (signed char x, unsigned long long y)
{
  return x << y;
}

short int
fn7 (short int x, unsigned long long y)
{
  return x << y;
}

int
fn8 (int x, unsigned long long y)
{
  return x << y;
}

long int
fn9 (long int x, unsigned long long y)
{
  return x << y;
}

long long int
fn10 (long long int x, unsigned long long y)
{
  return x << y;
}
