/* PR middle-end/95353 - spurious -Wstringop-overflow writing to a trailing
   array plus offset
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

struct S0 { char n, a[0]; };


void s0_nowarn_cstidx (struct S0 *p)
{
  char *q = p->a;
  q[1] = __LINE__;
  q[9] = __LINE__;
}

void s0_nowarn_cstoff_cstidx (struct S0 *p)
{
  char *q = p->a + 1;
  q[1] = __LINE__;
  q[9] = __LINE__;
}

void s0_nowarn_varoff_cstdix (struct S0 *p, int i)
{
  char *q = p->a + i;
  q[1] = __LINE__;            // { dg-bogus "\\\[-Wstringop-overflow" }
  q[9] = __LINE__;            // { dg-bogus "\\\[-Wstringop-overflow" }
}

void s0_nowarn_cstoff_varidx (struct S0 *p, int i)
{
  char *q = p->a + 1;
  q[i] = __LINE__;
}

void s0_nowarn_varoff_varidx (struct S0 *p, int i, int j)
{
  char *q = p->a + i;
  q[j] = __LINE__;            // { dg-bogus "\\\[-Wstringop-overflow" }
}


/* Accesses past the end of a trailing array with one element is
   discouraged but still reluctantly not diagnosed.  This should
   change.  */

struct S1 { char n, a[1]; };


void s1_nowarn_cstidx (struct S1 *p)
{
  char *q = p->a;
  q[1] = __LINE__;
  q[9] = __LINE__;
}

void s1_nowarn_cstoff_cstidx (struct S1 *p)
{
  char *q = p->a + 1;
  q[1] = __LINE__;
  q[9] = __LINE__;
}

void s1_nowarn_varoff_cstdix (struct S1 *p, int i)
{
  char *q = p->a + i;
  q[1] = __LINE__;            // { dg-bogus "\\\[-Wstringop-overflow" }
  q[9] = __LINE__;            // { dg-bogus "\\\[-Wstringop-overflow" }
}

void s1_nowarn_cstoff_varidx (struct S1 *p, int i)
{
  char *q = p->a + 1;
  q[i] = __LINE__;
}

void s1_nowarn_varoff_varidx (struct S1 *p, int i, int j)
{
  char *q = p->a + i;
  q[j] = __LINE__;
}


/* Accesses past the end of a trailing array with more than one
   element should be diagnosed but aren't yet because the MEM_REF
   makes the out-of-bounds accesses indistinguishable from valid
   ones to subsequent elements of the array pointed by P.  */

struct S2 { char n, a[2]; };


void s2_warn_cstidx (struct S2 *p)
{
  char *q = p->a;

  /* The following invalid store is represented as
       MEM[(char *)p_1(D) + 3B] = __LINE__;
     which is indistinguishable from the valid
       q = &p[1].n; q[0] = __LINE__;
  */
  q[2] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* } }
}

void s2_warn_cstoff_cstidx (struct S2 *p)
{
  char *q = p->a + 1;
  q[1] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* }  }
}

void s2_warn_varoff_cstdix (struct S2 *p, int i)
{
  char *q = p->a + i;
  q[2] = __LINE__;            // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
}

void s2_warn_cstoff_varidx (struct S2 *p, int i)
{
  char *q = p->a + 1;
  q[i] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* }  }
}

void s2_warn_varoff_varidx (struct S2 *p, int i, int j)
{
  char *q = p->a + i;
  q[j] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* }  }
}


/* Verify that none of these triggers a bogus warning (not tested
   elsewhere but triggered during bootstrap).  */

void s2_nowarn_varidx_int (struct S2 *p, int i)
{
  extern struct S2 s2;
  extern struct S2 s2a[];

  s2.a[i - 1] = __LINE__;
  s2.a[i] = __LINE__;
  s2.a[i + 1] = __LINE__;

  s2a[i].a[i - 1] = __LINE__;
  s2a[i].a[i] = __LINE__;
  s2a[i].a[i + 1] = __LINE__;

  p[i].a[i - 1] = __LINE__;
  p[i].a[i] = __LINE__;
  p[i].a[i + 1] = __LINE__;

  char *q = p->a;
  q[i - 1] = __LINE__;
  q[i] = __LINE__;
  q[i + 1] = __LINE__;
}

/* Same as above but with a size_t index in range [1, SIZE_MAX].  */

void* s2_nowarn_varidx_size (struct S2 *p, size_t i, size_t j)
{
  extern struct S2 s2;
  extern struct S2 s2a[];
  struct S2 *ps2 = __builtin_malloc (3 * sizeof *ps2);

  s2.a[i - 1] = __LINE__;
  s2.a[i] = __LINE__;
  s2.a[i + 1] = __LINE__;

  s2a[i].a[i - 1] = __LINE__;
  s2a[i].a[i] = __LINE__;
  s2a[i].a[i + 1] = __LINE__;

  p[i].a[i - 1] = __LINE__;
  p[i].a[i] = __LINE__;
  p[i].a[i + 1] = __LINE__;

  ps2->a[i - 1] = __LINE__;
  ps2->a[i] = __LINE__;
  ps2->a[i + 1] = __LINE__;

  char *q = p->a;
  q[i - 1] = __LINE__;
  q[i] = __LINE__;
  q[i + 1] = __LINE__;

  if (j == 0)
    return ps2;

  s2.a[j - 1] = __LINE__;
  s2.a[j] = __LINE__;
  s2.a[j + 1] = __LINE__;

  s2a[j].a[j - 1] = __LINE__;
  s2a[j].a[j] = __LINE__;
  s2a[j].a[j + 1] = __LINE__;

  p[j].a[j - 1] = __LINE__;
  p[j].a[j] = __LINE__;
  p[j].a[j + 1] = __LINE__;

  ps2->a[j - 1] = __LINE__;
  ps2->a[j] = __LINE__;
  ps2->a[j + 1] = __LINE__;

  q = p->a;
  q[j - 1] = __LINE__;
  q[j] = __LINE__;
  q[j + 1] = __LINE__;

  return ps2;
}

/* Verify that accesses to an interior zero-length array are diagnosed.  */

struct Si0 { char c, a[0], d; };

void si0_warn_cstidx (struct Si0 *p)
{
  // These are indistinguishable from valid accesses to p->d:
  //   MEM[(char *)p_1(D) + 1B] = 0;
  char *q = p->a;
  q[1] = __LINE__;            // { dg-warning "writing 1 byte into a region of size 0"  "pr?????" { xfail *-*-* } }
  q[9] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* } }
}

void si0_warn_cstoff_cstidx (struct Si0 *p)
{
  // Like those above, these too are indistinguishable from valid accesses
  // to p->d.
  char *q = p->a + 1;
  q[1] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* } }
  q[9] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* } }
}

void si0_warn_varoff_cstdix (struct Si0 *p, int i)
{
  char *q = p->a + i;
  q[1] = __LINE__;            // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  q[9] = __LINE__;            // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
}

void si0_warn_cstoff_varidx (struct Si0 *p, int i)
{
  char *q = p->a + 1;
  q[i] = __LINE__;            // { dg-warning "\\\[-Wstringop-overflow" "pr?????" { xfail *-*-* } }
}

void si0_warn_varoff_varidx (struct Si0 *p, int i, int j)
{
  char *q = p->a + i;
  q[j] = __LINE__;            // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
}
