/* Exercise conditional C-only uses of dangling pointers with optimization.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern void* memchr (const void*, int, size_t);
extern char* strchr (const char*, int);

void sink (void*, ...);


void nowarn_compound_literal (int i, int j)
{
  {
    int *p = i ? (int[]){ 1, 2, 3 } : (int[]){ 4, 5, 6 };
    sink (p);
  }
  {
    int a[] = { 1, 2, 3 };
    int *q = i ? (int[]){ 4, 5, 6 } : a;
    int *p = &q[1];
    sink (p);
  }
  {
    int *p = i ? (int[]){ 1, 2, 3 } : (int[]){ 4, 5, 6 };
    int *q = __builtin_memchr (p, 2, 3 * sizeof *p);
    sink (q);
  }
  {
    int a[] = { i, i + 1, i + 2, 3 };
    int *p = i ? (int[]){ j, j + 1, j + 2, 3 } : a;
    int *q = __builtin_memchr (p, 3, 4 * sizeof *p);
    sink (q);
  }
}


void warn_maybe_compound_literal (int i, int j)
{
  int a[] = { 1, 2, 3 }, *p;
  {
    p = i ? (int[]){ 4, 5, 6 } : a;
  }
  // When the 'p' is optimized away it's not mentioned in the warning.
  sink (p);         // { dg-warning "dangling pointer \('p' \)?to an unnamed temporary may be used" }
}


void warn_maybe_compound_literal_memchr (int i, int j, int x)
{
  int a[] = { 1, 2, 3 }, *p;
  {
    int *q = i ? (int[]){ 4, 5, 6 } : a;
    p = memchr (q, x, 3 * sizeof *q);
  }
  sink (p);         // { dg-warning "dangling pointer 'p' to an unnamed temporary may be used" }
}


void warn_maybe_array (int i, int j)
{
  int a[] = { 1, 2, 3 }, *p;
  {
    int b[] = { 4, 5, 6 };
    p = i ? a : b;
  }
  // When the 'p' is optimized away it's not mentioned in the warning.
  sink (p);         // { dg-warning "dangling pointer \('p' \)?to 'b' may be used" }
}


void warn_maybe_array_memchr (int i, int j, int x)
{
  int a[] = { 1, 2, 3 }, *p;
  {
    int b[] = { 4, 5, 6 };
    int *q = i ? a : b;
    p = memchr (q, x, 3 * sizeof *q);
  }
  sink (p);         // { dg-warning "dangling pointer 'p' to 'b' may be used" }
}
