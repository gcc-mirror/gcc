/* PR tree-optimization/87028 - false positive -Wstringop-truncation
   strncpy with global variable source string
   { dg-do compile }
   { dg-options "-O2 -Wstringop-truncation" } */

char *strncpy (char *, const char *, __SIZE_TYPE__);

#define STR   "1234567890"

struct S
{
  char a[5], b[5];
};

const char arr[] = STR;
const char* const ptr = STR;

const char arr2[][10] = { "123", STR };

void test_literal (struct S *s)
{
  strncpy (s->a, STR, sizeof s->a - 1);     /* { dg-bogus "\\\[-Wstringop-truncation]" } */
  s->a[sizeof s->a - 1] = '\0';
}

void test_global_arr (struct S *s)
{
  strncpy (s->a, arr, sizeof s->a - 1);     /* { dg-bogus "\\\[-Wstringop-truncation]" } */
  s->a [sizeof s->a - 1] = '\0';
}

void test_global_arr2 (struct S *s)
{
  strncpy (s->a, arr2[1], sizeof s->a - 1); /* { dg-bogus "\\\[-Wstringop-truncation]" } */
  s->a [sizeof s->a - 1] = '\0';

  strncpy (s->b, arr2[0], sizeof s->a - 1);
}

void test_global_ptr (struct S *s)
{
  strncpy (s->a, ptr, sizeof s->a - 1);     /* { dg-bogus "\\\[-Wstringop-truncation]" } */
  s->a [sizeof s->a - 1] = '\0';
}

void test_local_arr (struct S *s)
{
  const char arr[] = STR;
  strncpy (s->a, arr, sizeof s->a - 1);
  s->a [sizeof s->a - 1] = '\0';
}

void test_local_ptr (struct S *s)
{
  const char* const ptr = STR;
  strncpy (s->a, ptr, sizeof s->a - 1);     /* { dg-bogus "\\\[-Wstringop-truncation]" } */
  s->a [sizeof s->a - 1] = '\0';
}

void test_compound_literal (struct S *s)
{
  strncpy (s->a, (char[]){ STR }, sizeof s->a - 1);
  s->a [sizeof s->a - 1] = '\0';
}
