/* Test for proper comment elimination semantics from cpplib's -traditional.
   This should compile and link with compiled with `gcc -traditional-cpp'.
   Test case by Jason R. Thorpe <thorpej@zembu.com>.  */

/* { dg-do compile } */
/* { dg-options "-traditional-cpp" } */

#define A(name) X/**/name

#define B(name) \
void A(Y/**/name)() { A(name)(); }

void Xhello() { printf("hello world\n"); }

B(hello)

int main() { XYhello(); return (0); }
