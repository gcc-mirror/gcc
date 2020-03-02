/* PR middle-end/93829 - bogus -Wstringop-overflow on memcpy of a struct
   with a pointer member from another with a long string
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern void* memcpy (void*, const void*, __SIZE_TYPE__);

#define S40 "0123456789012345678901234567890123456789"

const char s40[] = S40;

struct S
{
  const void *p, *q, *r;
} s, sa[2];


void test_lit_decl (void)
{
  struct S t = { 0, S40, 0 };

  memcpy (&s, &t, sizeof t);    // { dg-bogus "-Wstringop-overflow" }
}

void test_str_decl (void)
{
  struct S t = { 0, s40, 0 };

  memcpy (&s, &t, sizeof t);    // { dg-bogus "-Wstringop-overflow" }
}


void test_lit_ssa (int i)
{
  if (i < 1)
    i = 1;
  struct S *p = &sa[i];
  struct S t = { 0, S40, 0 };

  memcpy (p, &t, sizeof t);    // { dg-bogus "-Wstringop-overflow" }
}

void test_str_ssa (int i)
{
  if (i < 1)
    i = 1;
  struct S *p = &sa[i];
  struct S t = { 0, s40, 0 };

  memcpy (p, &t, sizeof t);    // { dg-bogus "-Wstringop-overflow" }
}
