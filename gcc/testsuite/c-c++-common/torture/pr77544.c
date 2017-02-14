/* { dg-do compile } */

struct {
  long a : 17;
} b;
int c, d;
void e() { b.a = d + c + ~(long)(302806U >> 0); }
