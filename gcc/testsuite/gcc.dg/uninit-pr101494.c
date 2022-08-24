/* PR middle-end/101494 - bogus -Wmaybe-uninitialized on memrchr of size 0
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

typedef __SIZE_TYPE__ size_t;

void* alloca (size_t);

__attribute__ ((malloc, alloc_size (1))) void* alloc (size_t);

__attribute__ ((access (read_only, 1, 2))) void* sink (void*, size_t);

void test_alloca_zero (size_t i)
{
  char *p = alloca (0);
  sink (p, 0);      // { dg-bogus "\\\[-Wuninitialized" }
}

void test_alloca_zero_pi (size_t i)
{
  char *p = alloca (0);
  sink (p + i, 0);
}

void test_alloca_cst (void)
{
  char *p = alloca (7);
  sink (p, 0);      // { dg-bogus "\\\[-Wuninitialized" }
}

void test_alloca_cst_p1 (void)
{
  char *p = alloca (7);
  sink (p + 1, 0);  // { dg-bogus "\\\[-Wuninitialized" }
}

void test_alloca_cst_p7 (void)
{
  char *p = alloca (7);
  sink (p + 7, 0);  // { dg-bogus "\\\[-Wuninitialized" }
}

void test_alloca_var (size_t n)
{
  char *p = alloca (n);
  sink (p, 0);      // { dg-bogus "\\\[-Wuninitialized" }
}

void test_alloca_var_p1 (size_t n)
{
  char *p = alloca (n);
  sink (p + 1, 0);  // { dg-bogus "\\\[-Wuninitialized" }
}

void test_alloca_var_pn (size_t n)
{
  char *p = alloca (n);
  sink (p + n, 0);  // { dg-bogus "\\\[-Wuninitialized" }
}

