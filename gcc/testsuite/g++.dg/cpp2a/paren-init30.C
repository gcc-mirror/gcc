// PR c++/92812
// P1975R0
// { dg-do compile { target c++20 } }

struct S1 {
  int i;
  int j;
};

struct S2 {
  S1 s[4];
};

struct S3 {
  S2 s2;
};

void
f ()
{
  // Brace elision not allowed.
  auto s3 = static_cast<S3>(1); // { dg-error "could not convert" }
}
