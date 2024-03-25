/* { dg-do compile } */
/* { dg-options "-fharden-compares -fsignaling-nans -fnon-call-exceptions" } */

struct S
{
  S (bool);
  ~S ();
};

float f;

void
foo ()
{
  S a = 0;
  S b = f;
}
