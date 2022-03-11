/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

struct Data {
  char a;
  int b;
};

char c;

Data val(int idx) {
  return { c };  // { dg-warning "extended initializer" "c++ 98"  { target { c++98_only } } }
}

/* { dg-final { scan-assembler "movzbl" } } */
/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "movb" } } */

