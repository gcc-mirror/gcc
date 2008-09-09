// PR c++/37389
// { dg-do compile }
// { dg-options "-std=gnu++98" }

enum
{
  A = 9223372036854775807ULL * 2 + 1,
  B = B0,	// { dg-error "was not declared|overflow" }
  C = C0	// { dg-error "was not declared" }
};
