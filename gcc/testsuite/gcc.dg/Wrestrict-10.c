/* PR tree-optimization/84526 - ICE in generic_overlap
   { dg-do compile }
   { dg-options "-O2 -Wrestrict" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void* restrict, const void* restrict, size_t);
extern char* strcat (char* restrict, const char* restrict);
extern char* strcpy (char* restrict, const char* restrict);
extern char* strncat (char* restrict, const char* restrict, size_t);
extern char* strncpy (char* restrict, const char* restrict, size_t);

struct
{
  char a[1];
} b;

int i;
size_t n;

void __attribute__ ((noclone, noinline))
test_arr_memcpy_1 (void)
{
  memcpy (&b.a[i], b.a, n);
}

void __attribute__ ((noclone, noinline))
test_arr_memcpy_2 (void)
{
  memcpy (b.a, &b.a[i], n);
}

void __attribute__ ((noclone, noinline))
test_arr_strcat_1 (void)
{
  strcat (&b.a[i], b.a);            /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_arr_strcat_2 (void)
{
  strcat (b.a, &b.a[i]);            /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_arr_strncat_1 (void)
{
  strncat (&b.a[i], b.a, n);        /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_arr_strncat_2 (void)
{
  strncat (b.a, &b.a[i], n);        /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_arr_strcpy_1 (void)
{
  strcpy (&b.a[i], b.a);            /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_arr_strcpy_2 (void)
{
  strcpy (b.a, &b.a[i]);            /* { dg-warning "\\\[-Wrestrict" } */
}


struct S {
  int a;
  char b[10];
} d;

void __attribute__ ((noclone, noinline))
test_obj_memcpy_1 (void)
{
  memcpy (d.b, (char *) &d, n);
}

void __attribute__ ((noclone, noinline))
test_obj_memcpy_2 (void)
{
  memcpy ((char *) &d, d.b, n);
}

void __attribute__ ((noclone, noinline))
test_obj_strcpy_1 (void)
{
  strcpy (d.b, (char *) &d);
}

void __attribute__ ((noclone, noinline))
test_obj_strcpy_2 (void)
{
  strcpy ((char *) &d, d.b);
}

void __attribute__ ((noclone, noinline))
test_obj_strncat_1 (void)
{
  strncat (d.b, (char *) &d, n);    /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_obj_strncat_2 (void)
{
  strncat ((char *) &d, d.b, n);    /* { dg-warning "\\\[-Wrestrict" } */
}

void __attribute__ ((noclone, noinline))
test_obj_strncpy_1 (void)
{
  strncpy (d.b, (char *) &d, n);
}

void test_obj_strncpy_2 (void)
{
  strncpy ((char *) &d, d.b, n);
}
