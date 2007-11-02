// PR c++/33516
// { dg-do compile }

struct S1;
typedef S1 T1;
struct S1 {
  typedef int U;
  T1::U i;
};
struct S2;
typedef S2 T2;
struct S2 {
  typedef int U;
};
T2::U j;
struct S3;
typedef S3 T3;
struct S3 {
  typedef int U;
  S3::U i;
};

void
foo ()
{
  S1 s1;
  S2 s2;
  S3 s3;
  s1.i = 6;
  j = 7;
  s3.i = 8;
}
