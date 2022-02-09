/* PR middle-end/92942 - missing -Wstringop-overflow for allocations with
   a negative lower bound size
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

#define SIZE_MAX        __SIZE_MAX__
#define UINT8_MAX       __UINT8_MAX__
#define UINT16_MAX      __UINT16_MAX__

typedef __SIZE_TYPE__   size_t;
typedef __UINT8_TYPE__  uint8_t;
typedef __UINT16_TYPE__ uint16_t;

void* usr_alloc1 (size_t) __attribute__ ((alloc_size (1)));
void* usr_alloc2 (size_t, size_t)  __attribute__ ((alloc_size (1, 2)));

void* malloc (size_t);
void* memcpy (void*, const void*, size_t);
void* memset (void*, int, size_t);
char* strcpy (char*, const char*);

void sink (void*);

void malloc_uint_range_strcpy (unsigned n)
{
  void *p = malloc (5 < n ? 5 : n);

  strcpy (p, "01234");              // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);

  strcpy (p, "0123");
  sink (p);
}

void malloc_uint16_anti_range_memset (uint16_t n)
{
  if (5 <= n && n <= 9) return;
  void *p = malloc (n);

  if (UINT16_MAX < SIZE_MAX)
    {
      size_t sz = (uint16_t)-1 + (size_t)1;
      memset (p, 0, sz);            // { dg-warning "\\\[-Wstringop-overflow" }
      sink (p);
    }

  memset (p, 0, 1);
  sink (p);
  memset (p, 0, 5);
  sink (p);
  memset (p, 0, 6);
  sink (p);
  memset (p, 0, UINT16_MAX - 1);
  sink (p);
  memset (p, 0, UINT16_MAX);
  sink (p);
}

void malloc_int_strcpy (int n)
{
  void *p = malloc (7 < n ? 7 : n);

  strcpy (p, "0123456");            // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);

  strcpy (p, "012345");
  sink (p);
}

void vla_int_strcpy (int n)
{
  char a[9 < n ? 9 : n];

  strcpy (a, "012345678");          // { dg-warning "\\\[-Wstringop-overflow" }
  sink (a);

  strcpy (a, "01234567");
  sink (a);
}

void usr_alloc1_int_strcpy (int n)
{
  void *p = usr_alloc1 (7 < n ? 7 : n);

  strcpy (p, "0123456");            // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);

  strcpy (p, "012345");
  sink (p);
}

void usr_alloc2_cst_ir_strcpy (int n)
{
  void *p = usr_alloc2 (1, 5 < n ? 5 : n);

  strcpy (p, "01234");              // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);

  strcpy (p, "0123");
  sink (p);
}

void usr_alloc2_ir_ir_strcpy (int m, int n)
{
  void *p = usr_alloc2 (3 < n ? 3 : n, 5 < n ? 5 : n);

  strcpy (p, "0123456789abcde");    // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);

  strcpy (p, "0123456789abcd");
  sink (p);
}

void usr_alloc2_uint8_memset (uint8_t m, uint8_t n)
{
  if (3 <= m && m <= 7) return;
  if (5 <= n && n <= 9) return;
  void *p = usr_alloc2 (m, n);

  size_t sz = UINT8_MAX * UINT8_MAX + 1;
  memset (p, 0, sz);               // { dg-warning "\\\[-Wstringop-overflow" "" { xfail *-*-* } }
                                   // { dg-warning "\\\[-Warray-bounds" "pr?????" { target *-*-* } .-1 }
  sink (p);

  memset (p, 0, sz - 1);
  sink (p);
  memset (p, 0, 64);
  sink (p);
  memset (p, 0, 63);
  sink (p);
  memset (p, 0, 16);
  sink (p);
  memset (p, 0, 15);
  sink (p);
  memset (p, 0, 14);
  sink (p);
  memset (p, 0, 3);
  sink (p);
}



void malloc_int_memset (int n)
{
  void *p = malloc (11 < n ? 11 : n);

  memset (p, 0, 12);                // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);

  memset (p, 0, 11);
  sink (p);
}

void vla_int_memset (int n)
{
  char a[13 < n ? 13 : n];

  memset (a, 0, 14);                // { dg-warning "\\\[-Wstringop-overflow" }
  sink (a);

  memset (a, 0, 13);
  sink (a);
}
