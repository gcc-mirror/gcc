// Copy of abi/empty7.C.
// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }
// { dg-options "-fabi-version=0 -std=c++0x" }

struct S1 {};
struct S2 { virtual void f () {} S1 s1[4]; };
struct S3 : virtual public S2 {};
struct S4 : virtual public S2 { int i; };
struct S5 : public S3, virtual public S4 {};
struct S6 { S5 s5; };
struct S7 { S1 s1[5]; };
struct S8 : public S1, public S6, virtual public S7 { };

S8 s8;

int main () {
  if ((char *)(S7 *)&s8 - (char *)&s8 != 24)
    return 1;
}
