/* Exercise basic C-only cases of -Wdangling-pointer.
   { dg-do compile }
   { dg-options "-O0 -Wall" }
   { dg-require-effective-target alloca } */

typedef __SIZE_TYPE__ size_t;

extern void* memchr (const void*, int, size_t);
extern char* strchr (const char*, int);

void sink (const void*, ...);


void nowarn_compound_literal (int i)
{
  {
    int *p = (int[]){ 1, 2, 3 };
    sink (p);
  }
  {
    int *q = (int[]){ 1, 2, 3 };
    int *p = &q[1];
    sink (p);
  }
  {
    int *p = __builtin_memchr ((int[]){ 1, 2, 3 }, 2, 3 * sizeof *p);
    sink (p);
  }
  {
    int *p = __builtin_memchr ((int[]){ i, i + 1 }, 3, 2 * sizeof *p);
    sink (p);
  }
}


void warn_compound_literal (int i)
{
  int *p;
  {
    p = (int[]){ 1, 2, 3 };   // { dg-message "unnamed temporary" },
  }
  sink (p);                   // { dg-warning "using dangling pointer 'p' to an unnamed temporary" }

  {
    int *q =
      (int[]){ 1, 2, 3 };     // { dg-message "unnamed temporary" },
    p = &q[1];
  }
  sink (p);                   // { dg-warning "using dangling pointer 'p' to an unnamed temporary" }
  {
    p = (int*)memchr (
	  (int[]){ 1, 2, 3 }, // { dg-message "unnamed temporary" }
	  2, 3 * sizeof *p);
  }
  sink (p);                   // { dg-warning "using dangling pointer 'p' to an unnamed temporary" }

  {
    p = (int*)memchr (
	  (int[]){ i, i + 1 },// { dg-message "unnamed temporary" }
	  3, 2 * sizeof *p);
  }
  sink (p);                   // { dg-warning "using dangling pointer 'p' to an unnamed temporary" }
}


void warn_store_compound_literal (int **p)
{
  int *q = (int[]) { 1, 2, 3 };
  p[0] = q;                   // { dg-warning "storing the address" }
}

void warn_store_vla (int n, int **p)
{
  int a[n];
  p[1] = &a[1];               // { dg-warning "-Wdangling-pointer" "pr??????" { xfail *-*-* } }
}
