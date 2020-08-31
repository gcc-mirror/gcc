/* Verify -Wstringop-overread is issued for reading more than the maximum
   object size but not for writing.
  { dg-do compile }
  { dg-options "-O2 -Wno-stringop-overflow -ftrack-macro-expansion=0" } */

#define PTRDIFF_MAX   __PTRDIFF_MAX__
#define SIZE_MAX      __SIZE_MAX__

#define NOIPA         __attribute__ ((noipa))

typedef __SIZE_TYPE__ size_t;

void* memchr (const void*, int, size_t);
int memcmp (const void*, const void*, size_t);
void* memcpy (const void*, const void*, size_t);

int strncmp (const char*, const char*, size_t);
char* strncat (char*, const char*, size_t);
char* strncpy (char*, const char*, size_t);
size_t strnlen (const char*, size_t);

void sink (int, ...);
#define sink(...) sink (0, __VA_ARGS__)
#define T(exp)   sink (exp)

NOIPA void test_memchr (const void *p, int x)
{
  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (memchr (p, x, dmax));

  T (memchr (p, x, dmax + 1));     // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" }
  T (memchr (p, x, dmax * 2));     // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" }
  T (memchr (p, x, smax));         // { dg-warning "\\\[-Wstringop-overread" }
}


NOIPA void test_memcmp (const void *p, const void *q)
{
  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (memcmp (p, q, dmax));

  T (memcmp (p, q, dmax + 1));     // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" }
  T (memcmp (p, q, dmax * 2));     // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" }
  T (memcmp (p, q, smax));         // { dg-warning "\\\[-Wstringop-overread" }
}


NOIPA void test_memcpy (void *p, const void *q)
{
  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (memcpy (p, q, dmax));

  T (memcpy (p, q, dmax + 1));    // -Wstringop-overflow disabled
  T (memcpy (p, q, dmax * 2));    // ditto
  T (memcpy (p, q, smax));        // ditto
}


NOIPA void test_strncmp (const char *p, const char *q)
{
  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (strncmp (p, q, dmax));

  T (strncmp (p, q, dmax + 1));   // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" "strncmp" }
  T (strncmp (p, q, dmax * 2));   // { dg-warning "\\\[-Wstringop-overread" "strncmp" }
  T (strncmp (p, q, smax));       // { dg-warning "\\\[-Wstringop-overread" "strncmp" }
}

NOIPA void test_strncat (char *p, const char *q)
{
  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (strncat (p, q, dmax));

  T (strncat (p, q, dmax + 1));   // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" }
  T (strncat (p, q, dmax * 2));   // { dg-warning "\\\[-Wstringop-overread" }
  T (strncat (p, q, smax));       // { dg-warning "\\\[-Wstringop-overread" }
}

NOIPA void test_strncpy (char *p, const char *q)
{
#if 0
  /* Disabled: strncpy calls with an excissve bound trigger both
     -Wstringop-overflow and, when the former option is disabled,
     -Wstringop-overread.  The latter should probably not trigger.  */

  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (strncpy (p, q, dmax));

  T (strncpy (p, q, dmax + 1));    // -Wstringop-overflow disabled
  T (strncpy (p, q, dmax * 2));    // ditto
  T (strncpy (p, q, smax));        // ditto
#endif
}

NOIPA void test_strnlen (const char *p)
{
  size_t dmax = PTRDIFF_MAX;
  size_t smax = SIZE_MAX;

  T (strnlen (p, dmax));

  T (strnlen (p, dmax + 1));      // { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" }
  T (strnlen (p, dmax * 2));      // { dg-warning "\\\[-Wstringop-overread" }
  T (strnlen (p, smax));          // { dg-warning "\\\[-Wstringop-overread" }
}
