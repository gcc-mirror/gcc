/* { dg-do compile } */
/* { dg-options "-fno-short-enums -Wc++-compat" } */

enum E { A };

enum E v;
unsigned int *p = &v;	     /* { dg-warning "incompatible in C\[+\]\[+\]" } */

void foo(unsigned int);
void (*pfn)(enum E) = &foo;  /* { dg-warning "incompatible in C\[+\]\[+\]" } */
