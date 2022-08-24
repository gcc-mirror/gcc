/* PR tree-optimization/104119 - unexpected -Wformat-overflow after strlen
   in ILP32 since Ranger integration
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

void* malloc (size_t);
int sprintf (char*, const char*, ...);
size_t strlen (const char*);

void sink (void*, ...);

struct __attribute__ ((packed)) S
{
  char a3[3], a4[4], a5[5], a6[6], a7[7], a8[8], a9[9], ax[];
};

extern struct S s;
extern char a4[4], a7[7], a8[8];


void test_decl (void)
{
  struct S *p = &s;

  {
    size_t n = strlen (p->a3);
    sprintf (a4, "%s", p->a3);    // { dg-bogus "-Wformat-overflow" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a4);
    sprintf (a4, "%s", p->a4);    // { dg-bogus "-Wformat-overflow" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a5);
    sprintf (a4, "%s", p->a5);    // { dg-warning "may write a terminating nul past the end" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a7);
    sprintf (a8, "%s", p->a7);    // { dg-bogus "-Wformat-overflow" }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->a8);
    sprintf (a8, "%s", p->a8);    // { dg-bogus "-Wformat-overflow" }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->a9);
    sprintf (a8, "%s", p->a9);    // { dg-warning "may write a terminating nul past the end " }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->ax);
    sprintf (a7, "%s", p->ax);    // { dg-bogus "-Wformat-overflow" "pr??????" { xfail ilp32 } }
    sink (a7, n);
  }
}


/* Verify the warning with a pointer to an allocated object with nonstant
   size in known range.  */

void test_alloc_5_8 (int n)
{
  if (n < 5 || 8 < n)
    n = 5;

  struct S *p = (struct S*)malloc (sizeof *p + n);
  sink (p);   // initialize *p

  {
    size_t n = strlen (p->a3);
    sprintf (a4, "%s", p->a3);    // { dg-bogus "-Wformat-overflow" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a4);
    sprintf (a4, "%s", p->a4);    // { dg-bogus "-Wformat-overflow" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a5);
    sprintf (a4, "%s", p->a5);    // { dg-warning "may write a terminating nul past the end" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a7);
    sprintf (a8, "%s", p->a7);    // { dg-bogus "-Wformat-overflow" }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->a8);
    sprintf (a8, "%s", p->a8);    // { dg-bogus "-Wformat-overflow" }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->a9);
    sprintf (a8, "%s", p->a9);    // { dg-warning "may write a terminating nul past the end " }
    sink (a8, n);
  }

  {
    /* The size of the flexible array member p->ax is between 5 and 8
       bytes so the length of the string stored in it is at most 7.
       Verify the warning triggers based on its size and also gets
       the length right.  */
    size_t n = strlen (p->ax);
    sprintf (a4, "%s", p->ax);    // { dg-warning "writing up to 7 bytes " }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->ax);
    sprintf (a8, "%s", p->ax);
    sink (a8, n);
  }
}


void test_ptr (struct S *p)
{
  {
    size_t n = strlen (p->a3);
    sprintf (a4, "%s", p->a3);    // { dg-bogus "-Wformat-overflow" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a4);
    sprintf (a4, "%s", p->a4);    // { dg-bogus "-Wformat-overflow" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a5);
    sprintf (a4, "%s", p->a5);    // { dg-warning "may write a terminating nul past the end" }
    sink (a4, n);
  }

  {
    size_t n = strlen (p->a7);
    sprintf (a8, "%s", p->a7);    // { dg-bogus "-Wformat-overflow" }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->a8);
    sprintf (a8, "%s", p->a8);    // { dg-bogus "-Wformat-overflow" }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->a9);
    sprintf (a8, "%s", p->a9);    // { dg-warning "may write a terminating nul past the end " }
    sink (a8, n);
  }

  {
    size_t n = strlen (p->ax);
    sprintf (a8, "%s", p->ax);    // { dg-bogus "-Wformat-overflow" "pr??????" { xfail ilp32 } }
    sink (a8, n);
  }
}
