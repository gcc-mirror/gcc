/* PR middle-end/88771 - Misleading -Werror=array-bounds error
   Verify that the warning issued for calls to "bounded" string
   functions when -Wstringop-overflow is disabled is -Warray-bounds
   with the right wording.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-overflow -Wno-stringop-overread" } */

#define PTRDIFF_MAX   __PTRDIFF_MAX__
#define SIZE_MAX      __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);
extern void* memmove (void*, const void*, size_t);
extern void* memset (void*, int, size_t);

extern char* stpncpy (char*, const char*, size_t);

extern char* strncat (char*, const char*, size_t);
extern char* strncpy (char*, const char*, size_t);

extern char* strndup (const char*, size_t);

extern int strncmp (const char*, const char*, size_t);
extern int strncasecmp (const char*, const char*, size_t);

extern size_t strnlen (const char*, size_t);

extern char *d;
extern const char *s;


void test_memcpy (void)
{
  memcpy (d, s, SIZE_MAX);        /* { dg-warning ".memcpy. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" } */
}

void test_memmove (void)
{
  memmove (d, s, SIZE_MAX - 1);   /* { dg-warning ".memmove. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" } */
}

void test_memset (void)
{
  memset (d, 0, SIZE_MAX - 2);    /* { dg-warning ".memset. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" } */
}


char* test_stpncpy (void)
{
  return stpncpy (d, s, SIZE_MAX - 4);   /* { dg-warning ".stpncpy. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" } */
}


void test_strncat (void)
{
  strncat (d, s, SIZE_MAX - 3);   /* { dg-warning ".strncat. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" } */
}

void test_strncpy (void)
{
  strncpy (d, s, SIZE_MAX - 4);   /* { dg-warning ".strncpy. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" } */
}

char* test_strndup (void)
{
  return strndup (s, SIZE_MAX - 5);   /* { dg-warning ".strndup. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" "bug" { xfail *-*-* } } */
}

size_t test_strnlen (void)
{
  return strnlen (s, SIZE_MAX - 6);   /* { dg-warning ".strnlen. pointer overflow between offset 0 and size \[0-9\]+ \\\[-Warray-bounds" "bug" { xfail *-*-* } } */
}
