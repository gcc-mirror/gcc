/* PR middle-end/79223 - missing -Wstringop-overflow on a memmove overflow
   { dg-do compile }
   { dg-additional-options "-O2 -Wall -Wno-array-bounds -std=gnu99" }  */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);
extern void* mempcpy (void*, const void*, size_t);
extern void* memmove (void*, const void*, size_t);

char d[3];
char s[4];

size_t range (void)
{
  extern size_t size ();
  size_t n = size ();
  if (n <= sizeof d)
    return sizeof d + 1;

  return n;
}

void test_memcpy (void)
{
  memcpy (d, s, range ());   /* { dg-warning ".memcpy. writing 4 or more bytes into a region of size 3 overflows the destination" } */
}

void test_mempcpy (void)
{
  mempcpy (d, s, range ());   /* { dg-warning ".mempcpy. writing 4 or more bytes into a region of size 3 overflows the destination" } */
}

void test_memmove (void)
{
  memmove (d + 1, d, range ());   /* { dg-warning ".memmove. writing 4 or more bytes into a region of size 2 overflows the destination" } */
}
