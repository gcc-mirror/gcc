/* { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-overread" } */

typedef __SIZE_TYPE__ size_t;

extern size_t strlen (const char*);

void sink (size_t);

struct A0 { char i, a[0]; };

extern struct A0 ea0;

void fa0_extern (void)
{
  sink (strlen (ea0.a - 2));    // { dg-warning "\\\[-Warray-bounds" }
  sink (strlen (ea0.a - 1));    // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (ea0.a));        // valid just-past-the-end offset
  sink (strlen (ea0.a + 1));    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }
}

static struct A0 sa0 = { 0 };

void fa0_static (void)
{
  sink (strlen (sa0.a - 2));    // { dg-warning "\\\[-Warray-bounds" }
  sink (strlen (sa0.a - 1));    // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (sa0.a));        // valid just-past-the-end offset
  sink (strlen (sa0.a + 1));    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }
}


struct Ax { char i, a[]; };

extern struct Ax ax;

void fax_extern (void)
{
  sink (strlen (ax.a - 2));     // { dg-warning "\\\[-Warray-bounds" "pr93514" }
  sink (strlen (ax.a - 1));     // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (ax.a));
  sink (strlen (ax.a + 123));
}

static struct Ax ax0 = { 0, { 0 } };
static struct Ax ax1 = { 1, { 1, 0 } };
static struct Ax ax2 = { 2, { 2, 1, 0 } };
static struct Ax ax3 = { 3, { 3, 2, 1, 0 } };

void fax_static (void)
{
  sink (strlen (ax0.a - 2));    // { dg-warning "\\\[-Warray-bounds" }
  sink (strlen (ax0.a - 1));    // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (ax0.a));
  sink (strlen (ax0.a + 1));    // valid just-past-the-end offset
  sink (strlen (ax0.a + 2));    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }

  sink (strlen (ax1.a - 2));    // { dg-warning "\\\[-Warray-bounds" }
  sink (strlen (ax1.a - 1));    // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (ax1.a));
  sink (strlen (ax1.a + 1));
  sink (strlen (ax1.a + 2));    // valid just-past-the-end offset
  sink (strlen (ax1.a + 3));    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }

  sink (strlen (ax2.a - 2));    // { dg-warning "\\\[-Warray-bounds" }
  sink (strlen (ax2.a - 1));    // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (ax2.a));
  sink (strlen (ax2.a + 1));
  sink (strlen (ax2.a + 2));
  sink (strlen (ax2.a + 3));    // valid just-past-the-end offset
  sink (strlen (ax2.a + 4));    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }

  sink (strlen (ax3.a - 2));    // { dg-warning "\\\[-Warray-bounds" }
  sink (strlen (ax3.a - 1));    // { dg-warning "\\\[-Warray-bounds" "pr93514" { xfail *-*-* } }
  sink (strlen (ax3.a));
  sink (strlen (ax3.a + 1));
  sink (strlen (ax3.a + 2));
  sink (strlen (ax3.a + 3));
  sink (strlen (ax3.a + 4));    // valid just-past-the-end offset
  sink (strlen (ax3.a + 5));    // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }
}
