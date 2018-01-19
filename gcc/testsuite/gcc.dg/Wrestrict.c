/* Test to verify that VLAs are handled gracefully by -Wrestrict
   { dg-do compile }
   { dg-options "-O2 -Wrestrict" }
   { dg-require-effective-target alloca }  */

typedef __SIZE_TYPE__ size_t;

#define memcpy(d, s, n)  __builtin_memcpy (d, s, n)
#define strcpy(d, s)     __builtin_strcpy (d, s)

void test_vla (void *d, const char *s1, const char *s2, int i, size_t n)
{
  char a[n];
  char b[n];

  strcpy (a, s1);
  strcpy (b, s2);

  memcpy (d, i ? a : b, n);
}


void test_vla_member (void *d, const char *s1, const char *s2, int i, size_t n)
{
  struct S
  {
    char a[n];
    char b[n];
  } s;

  strcpy (s.a, s1);
  strcpy (s.b, s2);

  memcpy (d, i ? s.a : s.b, n);
}
