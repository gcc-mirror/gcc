/* PR middle-end/100571 - bogus -Wstringop-overflow with VLA of elements
   larger than byte
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

__attribute__ ((access (read_only, 1, 2))) void fro (int *, int);
__attribute__ ((access (write_only, 1, 2))) void fwo (int *, int);
__attribute__ ((access (read_write, 1, 2))) void frw (int *, int);

extern __SIZE_TYPE__ n;

void alloca_ro (void)
{
  int *a = __builtin_alloca (n * sizeof *a);
  a[0] = 0;
  fro (a, n);
}

void alloca_wo (void)
{
  int *a = __builtin_alloca (n * sizeof *a);
  fwo (a, n);
}

void alloca_rw (void)
{
  int *a = __builtin_alloca (n * sizeof *a);
  a[0] = 0;
  frw (a, n);
}


void calloc_ro (void)
{
  int *a = __builtin_calloc (n, sizeof *a);
  fro (a, n);
}

void calloc_wo (void)
{
  int *a = __builtin_calloc (n, sizeof *a);
  fwo (a, n);
}

void calloc_rw (void)
{
  int *a = __builtin_calloc (n, sizeof *a);
  a[0] = 0;
  frw (a, n);
}


void malloc_ro (void)
{
  int *a = __builtin_malloc (n * sizeof *a);
  a[0] = 0;
  fro (a, n);
}

void malloc_wo (void)
{
  int *a = __builtin_malloc (n * sizeof *a);
  fwo (a, n);
}

void malloc_rw (void)
{
  int *a = __builtin_malloc (n * sizeof *a);
  a[0] = 0;
  frw (a, n);
}


void vla_ro (void)
{
  int a[n];
  a[0] = 0;
  fro (a, n);
}

void vla_wo (void)
{
  int a[n];
  fwo (a, n);
}

void vla_rw (void)
{
  int a[n];
  a[0] = 0;
  frw (a, n);
}
