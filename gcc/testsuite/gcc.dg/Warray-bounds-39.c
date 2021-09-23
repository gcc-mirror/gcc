/* PR middle-end/88956 - ICE: Floating point exception on a memcpy from
   an zero-length constant array
   Verify both that memory and string calls with a zero-length array
   don't cause an ICE, and also that they emit warnings.
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */

#define NOIPA __attribute__ ((noipa))

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);
extern void* memmove (void*, const void*, size_t);
extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);

const char s0[0] = { };
const char s0_0[0][0] = { };
const char s0_1[0][1] = { };
const char s1_0[1][0] = { };

char d[4];

NOIPA void* test_memcpy_s0_1 (void *d)
{
  return memcpy (d, s0, 1);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_s0_2 (void *d)
{
  return memcpy (d, s0, 2);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_s0_0_1 (void *d)
{
  return memcpy (d, s0_0, 1);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_s0_0_2 (void *d)
{
  return memcpy (d, s0_0, 2);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}


NOIPA void* test_memcpy_s0_1_1 (void *d)
{
  return memcpy (d, s0_1, 1);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_s0_1_2 (void *d)
{
  return memcpy (d, s0_1, 2);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}


NOIPA void* test_memcpy_s1_0_1 (void *d)
{
  return memcpy (d, s1_0, 1);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_s1_0_2 (void *d)
{
  return memcpy (d, s1_0, 2);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}


NOIPA void* test_memmove_s0_1 (void *d)
{
  return memmove (d, s0, 1);      /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memmove_s0_2 (void *d)
{
  return memmove (d, s0, 2);      /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memmove_s0_0_1 (void *d)
{
  return memmove (d, s0_0, 1);    /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memmove_s0_0_2 (void *d)
{
  return memmove (d, s0_0, 2);    /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}


struct Empty { };
const struct Empty e = { };
const struct Empty e0[0] = { };
const struct Empty e0_0[0][0] = { };
const struct Empty e0_1[0][1] = { };
const struct Empty e1_0[1][0] = { };

NOIPA void* test_memcpy_e_1 (void *d)
{
  return memcpy (d, &e, 1);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_e0_1 (void *d)
{
  return memcpy (d, e0, 1);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_e0_0_1 (void *d)
{
  return memcpy (d, e0_0, 1);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_e0_1_1 (void *d)
{
  return memcpy (d, e0_1, 1);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA void* test_memcpy_e1_0_1 (void *d)
{
  return memcpy (d, e1_0, 1);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}


NOIPA char*
test_strcpy_s0 (char *d)          /* { dg-bogus "-Warray-bounds" "pr101679" { xfail *-*-* } } */
{
  return strcpy (d, s0);          /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA char* test_strcpy_s0_0 (char *d)
{
  return strcpy (d, s0_0[0]);     /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}


NOIPA char* test_strncpy_s0_1 (char *d)
{
  return strncpy (d, s0, 1);    /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA char* test_strncpy_s0_2 (char *d)
{
  return strncpy (d, s0, 2);    /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA char* test_strncpy_s0_0_1 (char *d)
{
  return strncpy (d, s0_0[0], 1); /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}

NOIPA char* test_strncpy_s0_0_2 (char *d)
{
  return strncpy (d, s0_0[0], 2); /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" } */
}
