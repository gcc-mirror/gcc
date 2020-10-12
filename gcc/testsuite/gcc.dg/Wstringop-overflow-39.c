/* PR middle-end/95667 - unintended warning for memset writing across multiple
   members
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

extern void sink (void*);

struct S1 { char a[3], b[5]; };

void warn_strcpy_s1 (void)
{
  struct S1 *p = __builtin_malloc (sizeof *p);
  char s[] = "1234567";
  __builtin_strcpy (p->a, s);         // { dg-warning "\\\[-Wstringop-overflow" }
  sink (p);
}

void nowarn_memset_s1 (void)
{
  struct S1 *p = __builtin_malloc (sizeof *p);
  __builtin_memset (p->a, 0, 8);      // { dg-bogus "\\\[-Wstringop-overflow" }
  sink (p);
}

struct S2 { char a[2], b[2][2], c[3]; };

void nowarn_memset_s2 (void)
{
  struct S2 *p = __builtin_malloc (sizeof *p);

  __builtin_memset (p->a, 0, sizeof *p);
  sink (p);

  __builtin_memset (p->b, 0, 7);
  sink (p);

  __builtin_memset (&p->b[0], 0, 7);
  sink (p);

  __builtin_memset (&p->b[1], 0, 5);
  sink (p);

  __builtin_memset (&p->b[0][0], 0, 7);
  sink (p);

  __builtin_memset (&p->b[0][1], 0, 6);
  sink (p);

  __builtin_memset (&p->b[1][0], 0, 5);
  sink (p);

  __builtin_memset (&p->b[1][1], 0, 4);
  sink (p);
}

void warn_memset_s2 (void)
{
  const unsigned n = sizeof (struct S2);
  struct S2 *p = __builtin_malloc (n);

  /* These should trigger -Wstringop-overflow rather than -Warray-bounds
     but the main purpose of the test is to verify the absence of warnings
     above so the exact warning for these overflwing calls isn't important
     here.  */

  __builtin_memset (p->a, 0, n + 1);  // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (p);

  __builtin_memset (p->b, 0, 8);      // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (p);

  __builtin_memset (&p->b[0], 0, 8);  // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (p);

  __builtin_memset (&p->b[0][0], 0, 8); // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (p);

  __builtin_memset (&p->b[1], 0, 6);  // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (p);

  __builtin_memset (&p->b[0][1], 0, 7); // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (p);
}

void nowarn_vl_struct (unsigned n)
{
  if (n < 3 || 5 < n)
    n = 3;

  struct V { char a[3], b[n], c[7]; } v;

  __builtin_memset (v.a, 0, 15);
  sink (&v);

  __builtin_memset (v.b, 0, 12);
  sink (&v);

  __builtin_memset (v.c, 0, 7);
  sink (&v);

  __builtin_memset (v.a, 0, 16);      // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (&v);

  __builtin_memset (v.b, 0, 13);      // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  sink (&v);

  /* The &V.C argument is represented as a variable offset from
     the beginning of the allocated object and there's no good
     way to figure out from its variable offset that it's base
     is the C member:
       s.1_12 = __builtin_alloca_with_align (prephitmp_24, 8);
       _9 = s.1_12 + prephitmp_27;
       __builtin_memset (_9, 0, 2);
  */

  __builtin_memset (v.c, 0, 8);       // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr?????" { xfail *-*-* } }
  sink (&v);
}
