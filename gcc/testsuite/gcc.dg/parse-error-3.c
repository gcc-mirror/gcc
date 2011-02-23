/* PR c/43384 */
/* { dg-do compile } */

void c_direct(par)
     void *par = &&lab; /* { dg-error "is initialized|non-standard|outside of" } */
{}

void foo(p, q)
     int *p = &q; /* { dg-error "initialized|undeclared" } */
{}

void bar(i)
     int j = i; /* { dg-error "initialized|undeclared|no such parameter" } */
{}
