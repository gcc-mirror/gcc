// { dg-do compile }
// { dg-options "-O2 -std=c++11 -fno-pic" }
// { dg-require-effective-target fpic }

struct dummy { };
struct true_type { struct dummy i; };

extern true_type y;
extern void xxx (true_type c);

void
yyy (void)
{
  xxx (y);
}

// { dg-final { scan-assembler "jmp\[\t \]+\[^\$\]*?_Z3xxx9true_type" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } }
