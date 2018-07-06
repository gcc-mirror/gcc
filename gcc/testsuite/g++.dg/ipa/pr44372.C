/* { dg-do compile } */
/* { dg-options "-O -fipa-cp -fipa-cp-clone" } */
/* { dg-additional-options "-Wno-return-type" } */

template < typename > class S3;

struct S1
{
  struct
  {
    int i[10];
  } s0;
  S1 () : s0 ()
  { }
  template < typename T > S1 (S3 < T > s3, int)
  {
    f (s3);
  }
};

struct S2
{
  template < typename T > S2 s (S3 < T > s3)
  {
    S1 (s3, 0);
  }
  S2 (int i) : j (i)
  { }
  int j;
  S1 s1[10];
};

template < typename > struct S3
{
  S3 ()
  {
    S2 (0).s (*this);
  }
};

static inline void
f (S3 < int > s3)
{
  extern bool m;
  if (m)
    S2 (0).s (s3);
}

S3 < int >s3;
