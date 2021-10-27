/* PR middle-end/91647 - missing -Warray-bounds accessing a zero-length array
   of a declared object
   { dg-do "compile" }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;

void sink (void*);

/* Exercise a true flexible member.  */

struct AX
{
  int32_t n;
  int16_t ax[];     // { dg-message "while referencing 'ax'" "member" }
};

static void warn_ax_local (struct AX *p)
{
  p->ax[0] = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->ax[1] = 1;     // { dg-warning "\\\[-Warray-bounds" }
}

static void nowarn_ax_extern (struct AX *p)
{
  p->ax[0] = 0; p->ax[99] = 99; p->ax[999] = 999; p->ax[9999] = 9999;
}

static void warn_ax_local_buf (struct AX *p)
{
  p->ax[0] = 4; p->ax[1] = 5;  // { dg-warning "\\\[-Wstringop-overflow" "pr102706" { target { vect_slp_v2hi_store &&  { ! vect_slp_v4hi_store } } } }

  p->ax[2] = 6;     // { dg-warning "\\\[-Warray-bounds" }
  p->ax[3] = 7;     // { dg-warning "\\\[-Warray-bounds" }
  p->ax[4] = 8;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_ax_extern_buf (struct AX *p)
{
  p->ax[0] = 9; p->ax[1] = 10; p->ax[2] = 11;

  p->ax[3] = 12;    // { dg-warning "\\\[-Warray-bounds" }
  p->ax[4] = 13;    // { dg-warning "\\\[-Warray-bounds" }
  p->ax[5] = 14;    // { dg-warning "\\\[-Warray-bounds" }
}

static void nowarn_ax_extern_bufx (struct AX *p)
{
  p->ax[0] = 0; p->ax[99] = 99; p->ax[999] = 999; p->ax[9999] = 9999;
}

static void nowarn_ax_ref (struct AX *p)
{
  p->ax[0] = 0; p->ax[99] = 99; p->ax[999] = 999; p->ax[9999] = 9999;
}

void test_ax (struct AX *p, unsigned n)
{
  {
    struct AX sax;  // { dg-message "defined here" "struct definition" }
    warn_ax_local (&sax);
    sink (&sax);
  }

  {
    extern
      struct AX xsax;
    nowarn_ax_extern (&xsax);
    sink (&xsax);
  }

  {
    /* Verify out-of-bounds access to the local BUF is diagnosed.  */
    char ax_buf_p2[sizeof (struct AX) + 2 * sizeof (int16_t)];
    warn_ax_local_buf ((struct AX*) ax_buf_p2);
    sink (ax_buf_p2);
  }

  {
    /* Verify out-of-bounds access to the extern BUF with a known
       bound is diagnosed.  */
    extern char ax_buf_p3[sizeof (struct AX) + 3 * sizeof (int16_t)];
    warn_ax_extern_buf ((struct AX*) ax_buf_p3);
    sink (ax_buf_p3);
  }

  {
    /* Verify that accesses to BUFX with an unknown bound are not
       diagnosed.  */
    extern char bufx[];
    nowarn_ax_extern_bufx ((struct AX*) bufx);
    sink (bufx);
  }

  {
    /* Verify that accesses to BUFN with a runtime bound are not
       diagnosed.  */
    char bufn[n];
    nowarn_ax_extern_bufx ((struct AX*) bufn);
    sink (bufn);
  }

  nowarn_ax_ref (p);
}


/* Exercise a zero-length trailing member array.  It's the same as above
   except that extern declarations with no definitions are considered to
   have zero elements (they can't be initialized to have any).  */

struct A0
{
  int32_t n;
  int16_t a0[0];    // { dg-message "while referencing 'a0'" "member" }
};

static void warn_a0_local (struct A0 *p)
{
  p->a0[0] = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->a0[1] = 1;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a0_extern (struct A0 *p)
{
  p->a0[0] = 2;     // { dg-warning "\\\[-Warray-bounds" }
  p->a0[1] = 3;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a0_local_buf (struct A0 *p)
{
  p->a0[0] = 4; p->a0[1] = 5;  // { dg-warning "\\\[-Wstringop-overflow" "pr102706" { target { vect_slp_v2hi_store && { ! vect_slp_v4hi_store } } } }

  p->a0[2] = 6;     // { dg-warning "\\\[-Warray-bounds" }
  p->a0[3] = 7;     // { dg-warning "\\\[-Warray-bounds" }
  p->a0[4] = 8;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a0_extern_buf (struct A0 *p)
{
  p->a0[0] = 9; p->a0[1] = 10; p->a0[2] = 11;

  p->a0[3] = 12;    // { dg-warning "\\\[-Warray-bounds" }
  p->a0[4] = 13;    // { dg-warning "\\\[-Warray-bounds" }
  p->a0[5] = 14;    // { dg-warning "\\\[-Warray-bounds" }
}

static void nowarn_a0_extern_bufx (struct A0 *p)
{
  p->a0[0] = 0; p->a0[99] = 99; p->a0[999] = 999; p->a0[9999] = 9999;
}

static void nowarn_a0_ref (struct A0 *p)
{
  p->a0[0] = 0; p->a0[99] = 99; p->a0[999] = 999; p->a0[9999] = 9999;
}

void test_a0 (struct A0 *p, unsigned n)
{
  {
    struct A0 sa0;  // { dg-message "defined here" "struct definition" }
    warn_a0_local (&sa0);
    sink (&sa0);
  }

  {
    extern
      struct A0 xsa0;  // { dg-message "defined here" "struct definition" }
    warn_a0_extern (&xsa0);
    sink (&xsa0);
  }

  {
    /* Verify out-of-bounds access to the local BUF is diagnosed.  */
    char a0_buf_p2[sizeof (struct A0) + 2 * sizeof (int16_t)];
    warn_a0_local_buf ((struct A0*) a0_buf_p2);
    sink (a0_buf_p2);
  }

  {
    /* Verify out-of-bounds access to the extern BUF with a known
       bound is diagnosed.  */
    extern char a0_buf_p3[sizeof (struct A0) + 3 * sizeof (int16_t)];
    warn_a0_extern_buf ((struct A0*) a0_buf_p3);
    sink (a0_buf_p3);
  }

  {
    /* Verify that accesses to BUFX with an unknown bound are not
       diagnosed.  */
    extern char bufx[];
    nowarn_a0_extern_bufx ((struct A0*) bufx);
    sink (bufx);
  }

  {
    /* Verify that accesses to BUFN with a runtime bound are not
       diagnosed.  */
    char bufn[n];
    nowarn_a0_extern_bufx ((struct A0*) bufn);
    sink (bufn);
  }

  nowarn_a0_ref (p);
}


/* Exercise a one-element trailing member array.  It's the same as above
   except that it has exactly one element.  */

struct A1
{
  int32_t n;
  int16_t a1[1];    // { dg-message "while referencing 'a1'" }
};

static void warn_a1_local_noinit (struct A1 *p)
{
  p->a1[0] = 0;
  p->a1[1] = 1;     // { dg-warning "\\\[-Warray-bounds" }
  p->a1[2] = 2;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a1_extern (struct A1 *p)
{
  p->a1[0] = 0;
  p->a1[1] = 1;     // { dg-warning "\\\[-Warray-bounds" }
  p->a1[2] = 2;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a1_init (struct A1 *p)
{
  p->a1[0] = 0;
  p->a1[1] = 1;     // { dg-warning "\\\[-Warray-bounds" }
  p->a1[2] = 2;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a1_local_buf (struct A1 *p)
{
  p->a1[0] = 0; p->a1[1] = 1; p->a1[2] = 2; p->a1[3] = 3;

  p->a1[4] = 4;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a1_extern_buf (struct A1 *p)
{
  p->a1[0] = 0; p->a1[1] = 1; p->a1[2] = 2; p->a1[3] = 3; p->a1[4] = 4;

  p->a1[5] = 5;     // { dg-warning "\\\[-Warray-bounds" }
}

static void nowarn_a1_extern_bufx (struct A1 *p)
{
  p->a1[0] = 0; p->a1[99] = 99; p->a1[999] = 999; p->a1[9999] = 9999;
}

static void nowarn_a1_ref (struct A1 *p)
{
  p->a1[0] = 0; p->a1[99] = 99; p->a1[999] = 999; p->a1[9999] = 9999;
}

void test_a1 (struct A1 *p, unsigned n)
{
  {
    struct A1 a1;
    warn_a1_local_noinit (&a1);
    sink (&a1);
  }

  {
    extern struct A1 a1x;
    warn_a1_extern (&a1x);
    sink (&a1x);
}
  {
    struct A1 a1 = { 0, { 1 } };
    warn_a1_init (&a1);
    sink (&a1);
  }

  {
    /* Verify out-of-bounds access to the local BUF is diagnosed.  */
    char buf_p2[sizeof (struct A1) + 2 * sizeof (int16_t)];
    warn_a1_local_buf ((struct A1*) buf_p2);
    sink (buf_p2);
  }

  {
    /* Verify out-of-bounds access to the extern BUF with a known
       bound is diagnosed.  */
    extern char a1_buf_p3[sizeof (struct A1) + 3 * sizeof (int16_t)];
    warn_a1_extern_buf ((struct A1*) a1_buf_p3);
    sink (a1_buf_p3);
  }

  {
    /* Verify that accesses to BUFX with an unknown bound are not
       diagnosed.  */
    extern char bufx[];
    nowarn_a1_extern_bufx ((struct A1*) bufx);
    sink (bufx);
  }

  {
    /* Verify that accesses to BUFN with a runtime bound are not
       diagnosed.  */
    char bufn[n];
    nowarn_a1_extern_bufx ((struct A1*) bufn);
    sink (bufn);
  }

  nowarn_a1_ref (p);
}


/* Exercise a two-element trailing member array.  It's treated
   the same as an interior array member.  */

struct A2
{
  int32_t n;
  int16_t a2[2];    // { dg-message "while referencing 'a2'" }
};

static void warn_a2_noinit (struct A2 *p)
{
  p->a2[0] = 0; p->a2[1] = 1;

  p->a2[2] = 2;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a2_init (struct A2 *p)
{
  p->a2[0] = 0; p->a2[1] = 1;

  p->a2[2] = 2;     // { dg-warning "\\\[-Warray-bounds" }
  p->a2[9] = 9;     // { dg-warning "\\\[-Warray-bounds" }
}

static void warn_a2_ref (struct A2 *p)
{
  p->a2[0] = 0; p->a2[1] = 1;

  p->a2[2] = 2;     // { dg-warning "\\\[-Warray-bounds" }
  p->a2[9] = 9;     // { dg-warning "\\\[-Warray-bounds" }
}

void test_a2 (struct A2 *p)
{
  {
    struct A2 a2;
    warn_a2_noinit (&a2);
    sink (&a2);
  }

  {
    struct A2 a2 = { 0, { 1, 2 } };
    warn_a2_init (&a2);
    sink (&a2);
  }

  warn_a2_ref (p);
}
