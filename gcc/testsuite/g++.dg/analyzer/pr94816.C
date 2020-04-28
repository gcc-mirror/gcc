/* { dg-additional-options "-O" } */

struct jr;

struct ch {
  int jr::*rx;
};

ch
ad ()
{
  return ch ();
}
