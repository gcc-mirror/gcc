// { dg-do compile { target i?86-*-* } }

struct A
{
};

struct B : public A
{
  char b[0x20000000];
} e;
