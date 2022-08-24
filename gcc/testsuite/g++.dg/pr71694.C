/* { dg-do compile } */
/* { dg-options "-O2 -fno-store-merging" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-fno-common -mdynamic-no-pic" { target { ia32 && { x86_64-*-darwin* i?86-*-darwin* } } } } */

struct B {
    B() {}
    int x;
    int a : 6;
    int b : 6;
    int c : 6;
};

struct C : B {
    char d;
};

C c;

int main()
{
  /* We have to make sure to not cause a store data race between
     c.c and c.d residing in the tail padding of B.  */
  c.c = 1;
  c.d = 2;
}

/* In particular on x86 c.d should not be loaded/stored via movl.  */
/* { dg-final { scan-assembler-not "movl" { target { x86_64-*-* i?86-*-* } } } } */
