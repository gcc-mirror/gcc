// DR 1397
// { dg-require-effective-target c++11 }

struct A
{
  int i = sizeof(A{});		// { dg-error "" }
};
