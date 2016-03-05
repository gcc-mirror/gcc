/* { dg-do compile } */
/* { dg-options "-w" } */
int bar() { return foo(); }
void baz(int c[foo()]) { return; }
