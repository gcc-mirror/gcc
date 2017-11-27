/* Verify that 
   { dg-do compile }
   { dg-options "-O2 -Wstringop-truncation -Wno-stringop-overflow -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

#define stpncpy(d, s, n) __builtin_stpncpy ((d), (s), (n))
#define strncpy(d, s, n) __builtin_stpncpy ((d), (s), (n))

void sink (void*);

struct A {
  char arr[3] __attribute__ ((nonstring));
  char str[3];
};

struct B { struct A a[3]; int i; };
struct C { struct B b[3]; int i; };

void stpncpy_arr_1 (struct C *pc, const char *s)
{
  stpncpy (pc->b[0].a[0].arr, s, sizeof pc->b[0].a[0].arr);
  sink (pc->b[0].a[0].arr);

  stpncpy (pc->b[0].a[1].arr, s, sizeof pc->b[0].a[1].arr);
  sink (pc->b[0].a[1].arr);

  stpncpy (pc->b[0].a[2].arr, s, sizeof pc->b[0].a[2].arr);
  sink (pc->b[0].a[2].arr);

  stpncpy (pc->b[1].a[0].arr, s, sizeof pc->b[1].a[0].arr);
  sink (pc->b[1].a[0].arr);

  stpncpy (pc->b[1].a[1].arr, s, sizeof pc->b[1].a[1].arr);
  sink (pc->b[1].a[1].arr);

  stpncpy (pc->b[1].a[2].arr, s, sizeof pc->b[1].a[2].arr);
  sink (pc->b[1].a[2].arr);

  stpncpy (pc->b[2].a[0].arr, s, sizeof pc->b[2].a[0].arr);
  sink (pc->b[2].a[0].arr);

  stpncpy (pc->b[2].a[1].arr, s, sizeof pc->b[2].a[1].arr);
  sink (pc->b[2].a[1].arr);

  stpncpy (pc->b[2].a[2].arr, s, sizeof pc->b[2].a[2].arr);
  sink (pc->b[2].a[2].arr);
}

void stpncpy_str_nowarn_1 (struct C *pc, const char *s)
{
  stpncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str)[-1] = 0;   /* { dg-bogus "\\\[-Wstringop-truncation" } */
}

void stpncpy_str_nowarn_2 (struct C *pc, const char *s)
{
  *stpncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str - 1) = 0;   /* { dg-bogus "\\\[-Wstringop-truncation" } */
}

void stpncpy_str_nowarn_3 (struct C *pc, const char *s)
{
  char *d = stpncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str);   /* { dg-bogus "\\\[-Wstringop-truncation" } */

  d[-1] = 0;
}

void stpncpy_str_nowarn_4 (struct C *pc, const char *s)
{
  char *d = stpncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str - 1);   /* { dg-bogus "\\\[-Wstringop-truncation" } */

  *d = 0;
}

void strncpy_arr_1 (struct C *pc, const char *s)
{
  strncpy (pc->b[0].a[0].arr, s, sizeof pc->b[0].a[0].arr);
  sink (pc->b[0].a[0].arr);

  strncpy (pc->b[0].a[1].arr, s, sizeof pc->b[0].a[1].arr);
  sink (pc->b[0].a[1].arr);

  strncpy (pc->b[0].a[2].arr, s, sizeof pc->b[0].a[2].arr);
  sink (pc->b[0].a[2].arr);
}

void strncpy_str_nowarn_1 (struct C *pc, const char *s)
{
  strncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str);   /* { dg-bogus "\\\[-Wstringop-truncation" } */

  pc->b[0].a[0].str[sizeof pc->b[0].a[0].str - 1] = 0;
}

void strncpy_str_warn_1 (struct C *pc, const char *s)
{
  strncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str);   /* { dg-warning "specified bound 3 equals destination size" } */

  pc->b[1].a[0].str[sizeof pc->b[0].a[0].str - 1] = 0;
}

void strncpy_str_warn_2 (struct C *pc, const char *s)
{
  strncpy (pc->b[0].a[0].str, s, sizeof pc->b[0].a[0].str);   /* { dg-warning "specified bound 3 equals destination size" } */

  pc->b[0].a[1].str[sizeof pc->b[0].a[0].str - 1] = 0;
}
