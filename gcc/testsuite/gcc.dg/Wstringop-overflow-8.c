/* PR tree-optimization/79220 - missing -Wstringop-overflow= on a memcpy
   overflow with a small power-of-2 size
   { dg-do compile }
   { dg-options "-O2 -Wno-array-bounds -Wstringop-overflow" } */

extern void* memcpy (void*, const void*, __SIZE_TYPE__);
extern void* memmove (void*, const void*, __SIZE_TYPE__);
extern void* memset (void*, int, __SIZE_TYPE__);

char d[1];

void test_memcpy_lit_2 (void)
{
  memcpy (d, "01", 2);          /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memcpy_lit_4 (void)
{
  memcpy (d, "0123", 4);        /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memmove_lit_8 (void)
{
  memmove (d, "01234567", 8);   /* { dg-warning "\\\[-Wstringop-overflow" } */
}


void test_memcpy_ptr_2 (const void *s)
{
  memcpy (d, s, 2);             /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memcpy_ptr_4 (const void *s)
{
  memcpy (d, s, 4);             /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memcpy_ptr_8 (const void *s)
{
  memcpy (d, s, 8);             /* { dg-warning "\\\[-Wstringop-overflow" } */
}


void test_memmove_ptr (const void *s)
{
  memmove (d, s, 8);            /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memset_2 (void)
{
  memset (d, 0, 2);             /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memset_4 (void)
{
  memset (d, 0, 4);             /* { dg-warning "\\\[-Wstringop-overflow" } */
}

void test_memset_8 (void)
{
  memset (d, 0, 8);             /* { dg-warning "\\\[-Wstringop-overflow" } */
}
