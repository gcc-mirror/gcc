/* Use -g0 so that this test case doesn't just fail because
   of PR52472.  */

/* { dg-do compile } */
/* { dg-options "-std=gnu99 -g0" } */

struct S12
{
  char c;
  const char *p;
};

struct S12f
{
  char c;
  struct S12f (*f)(void);
};

struct S12labl
{
  char c;
  void **labl;
};

struct S121
{
  char c;
  const char *p;
  char d;
};

const char str[5] = "abcd";

struct S12 test_S12_0 (void)
{
  struct S12 s;
  s.c = 'A';
  s.p = str;
  return s;
}

struct S12 test_S12_4 (void)
{
  struct S12 s;
  s.c = 'A';
  s.p = str + 4;
  return s;
}

struct S12f test_S12f (void)
{
  struct S12f s;
  s.c = 'A';
  s.f = test_S12f;
  return s;
}

struct S121 test_S121 (void)
{
  struct S121 s;
  s.c = 'c';
  s.p = str + 4;
  s.d = 'd';
  return s;
}

extern void use_S12lab (struct S12labl*);

struct S12labl test_S12lab (void)
{
  struct S12labl s;
labl:;
  s.c = 'A';
  s.labl = &&labl;
  return s;
}

#ifdef __MEMX

struct S13
{
  char c;
  const __memx char *p;
};

const __memx char str_x[] = "abcd";

struct S13 test_S13_0 (void)
{
  struct S13 s;
  s.c = 'A';
  s.p = str_x;
  return s;
}

struct S13 test_S13_4a (void)
{
  struct S13 s;
  s.c = 'A';
  s.p = str_x + 4;
  return s;
}

#ifdef __FLASH1

const __flash1 char str_1[] = "abcd";

struct S13 test_13_4b (void)
{
  struct S13 s;
  s.c = 'A';
  s.p = str_1 + 4;
  return s;
}

#endif /* have __flash1 */
#endif /* have __memx */

