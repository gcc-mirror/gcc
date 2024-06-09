/* { dg-do compile } */
/* { dg-options "-fpermissive -w" } */
int bar() { return foo(); }
void baz(int c[foo()]) { return; }
