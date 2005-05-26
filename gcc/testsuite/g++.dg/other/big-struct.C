// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }

struct A
{
};

struct B : public A
{
  char b[0x20000000];
} e;
