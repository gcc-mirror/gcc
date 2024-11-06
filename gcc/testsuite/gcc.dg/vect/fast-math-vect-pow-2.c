/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

typedef double d_type;
struct
{
  d_type x;
  d_type y;
} S[100];

#define N 16
d_type foo (d_type t);

d_type
main1 ()
{
  int i;
  d_type t;

  for (i = 0; i < N; i++)
    {
      t = (d_type) i / (d_type) 10;
      S[5].x = t * t;
    }
  return S[5].x;
}

int
main (void)
{
  d_type tmp = main1 ();
}

