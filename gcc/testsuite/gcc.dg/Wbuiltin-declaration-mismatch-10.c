/* PR c/86308 - ICE in verify_gimple calling an invalid index() declaration
   { dg-do compile }
   { dg-options "-Wall" } */

int index (int, int);         /* { dg-warning "conflicting types for built-in function .index.; expected .char \\\*\\\(const char \\\*, int\\\)." } */

int foo (const short *a)
{
  return a[index (0, 0)];
}
