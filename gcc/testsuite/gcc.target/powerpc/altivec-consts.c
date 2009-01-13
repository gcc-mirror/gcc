/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mabi=altivec -O2" } */

/* Check that "easy" AltiVec constants are correctly synthesized.  */

extern void abort (void);

typedef __attribute__ ((vector_size (16))) unsigned char v16qi;
typedef __attribute__ ((vector_size (16))) unsigned short v8hi;
typedef __attribute__ ((vector_size (16))) unsigned int v4si;

char w[16] __attribute__((aligned(16)));
 

/* Emulate the vspltis? instructions on a 16-byte array of chars.  */

void vspltisb (char *v, int val)
{
  int i;
  for (i = 0; i < 16; i++)
    v[i] = val;
}

void vspltish (char *v, int val)
{
  int i;
  for (i = 0; i < 16; i += 2)
    v[i] = val >> 7, v[i + 1] = val;
}

void vspltisw (char *v, int val)
{
  int i;
  for (i = 0; i < 16; i += 4)
    v[i] = v[i + 1] = v[i + 2] = val >> 7, v[i + 3] = val;
}


/* Use three different check functions for each mode-instruction pair.
   The callers have no typecasting and no addressable vectors, to make
   the test more robust.  */

void __attribute__ ((noinline)) check_v16qi (v16qi v1, char *v2)
{
  if (memcmp (&v1, v2, 16))
    abort ();
}

void __attribute__ ((noinline)) check_v8hi (v8hi v1, char *v2)
{
  if (memcmp (&v1, v2, 16))
    abort ();
}

void __attribute__ ((noinline)) check_v4si (v4si v1, char *v2)
{
  if (memcmp (&v1, v2, 16))
    abort ();
}


/* V16QI tests.  */

void v16qi_vspltisb ()
{
  v16qi v = { 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15 };
  vspltisb (w, 15);
  check_v16qi (v, w);
}

void v16qi_vspltisb_neg ()
{
  v16qi v = { -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5 };
  vspltisb (w, -5);
  check_v16qi (v, w);
}

void v16qi_vspltisb_addself ()
{
  v16qi v = { 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 };
  vspltisb (w, 30);
  check_v16qi (v, w);
}

void v16qi_vspltisb_neg_addself ()
{
  v16qi v = { -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24 };
  vspltisb (w, -24);
  check_v16qi (v, w);
}

void v16qi_vspltish ()
{
  v16qi v = { 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15 };
  vspltish (w, 15);
  check_v16qi (v, w);
}

void v16qi_vspltish_addself ()
{
  v16qi v = { 0, 30, 0, 30, 0, 30, 0, 30, 0, 30, 0, 30, 0, 30, 0, 30 };
  vspltish (w, 30);
  check_v16qi (v, w);
}

void v16qi_vspltish_neg ()
{
  v16qi v = { -1, -5, -1, -5, -1, -5, -1, -5, -1, -5, -1, -5, -1, -5, -1, -5 };
  vspltish (w, -5);
  check_v16qi (v, w);
}

void v16qi_vspltisw ()
{
  v16qi v = { 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15 };
  vspltisw (w, 15);
  check_v16qi (v, w);
}

void v16qi_vspltisw_addself ()
{
  v16qi v = { 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30 };
  vspltisw (w, 30);
  check_v16qi (v, w);
}

void v16qi_vspltisw_neg ()
{
  v16qi v = { -1, -1, -1, -5, -1, -1, -1, -5, -1, -1, -1, -5, -1, -1, -1, -5 };
  vspltisw (w, -5);
  check_v16qi (v, w);
}


/* V8HI tests. */

void v8hi_vspltisb ()
{
  v8hi v = { 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F };
  vspltisb (w, 15);
  check_v8hi (v, w);
}

void v8hi_vspltisb_addself ()
{
  v8hi v = { 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E };
  vspltisb (w, 30);
  check_v8hi (v, w);
}

void v8hi_vspltisb_neg ()
{
  v8hi v = { 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB };
  vspltisb (w, -5);
  check_v8hi (v, w);
}

void v8hi_vspltish ()
{
  v8hi v = { 15, 15, 15, 15, 15, 15, 15, 15 };
  vspltish (w, 15);
  check_v8hi (v, w);
}

void v8hi_vspltish_neg ()
{
  v8hi v = { -5, -5, -5, -5, -5, -5, -5, -5 };
  vspltish (w, -5);
  check_v8hi (v, w);
}

void v8hi_vspltish_addself ()
{
  v8hi v = { 30, 30, 30, 30, 30, 30, 30, 30 };
  vspltish (w, 30);
  check_v8hi (v, w);
}

void v8hi_vspltish_neg_addself ()
{
  v8hi v = { -24, -24, -24, -24, -24, -24, -24, -24 };
  vspltish (w, -24);
  check_v8hi (v, w);
}

void v8hi_vspltisw ()
{
  v8hi v = { 0, 15, 0, 15, 0, 15, 0, 15 };
  vspltisw (w, 15);
  check_v8hi (v, w);
}

void v8hi_vspltisw_addself ()
{
  v8hi v = { 0, 30, 0, 30, 0, 30, 0, 30 };
  vspltisw (w, 30);
  check_v8hi (v, w);
}

void v8hi_vspltisw_neg ()
{
  v8hi v = { -1, -5, -1, -5, -1, -5, -1, -5 };
  vspltisw (w, -5);
  check_v8hi (v, w);
}

/* V4SI tests. */

void v4si_vspltisb ()
{
  v4si v = { 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F };
  vspltisb (w, 15);
  check_v4si (v, w);
}

void v4si_vspltisb_addself ()
{
  v4si v = { 0x1E1E1E1E, 0x1E1E1E1E, 0x1E1E1E1E, 0x1E1E1E1E };
  vspltisb (w, 30);
  check_v4si (v, w);
}

void v4si_vspltisb_neg ()
{
  v4si v = { 0xFBFBFBFB, 0xFBFBFBFB, 0xFBFBFBFB, 0xFBFBFBFB };
  vspltisb (w, -5);
  check_v4si (v, w);
}

void v4si_vspltish ()
{
  v4si v = { 0x000F000F, 0x000F000F, 0x000F000F, 0x000F000F };
  vspltish (w, 15);
  check_v4si (v, w);
}

void v4si_vspltish_addself ()
{
  v4si v = { 0x001E001E, 0x001E001E, 0x001E001E, 0x001E001E };
  vspltish (w, 30);
  check_v4si (v, w);
}

void v4si_vspltish_neg ()
{
  v4si v = { 0xFFFBFFFB, 0xFFFBFFFB, 0xFFFBFFFB, 0xFFFBFFFB };
  vspltish (w, -5);
  check_v4si (v, w);
}

void v4si_vspltisw ()
{
  v4si v = { 15, 15, 15, 15 };
  vspltisw (w, 15);
  check_v4si (v, w);
}

void v4si_vspltisw_neg ()
{
  v4si v = { -5, -5, -5, -5 };
  vspltisw (w, -5);
  check_v4si (v, w);
}

void v4si_vspltisw_addself ()
{
  v4si v = { 30, 30, 30, 30 };
  vspltisw (w, 30);
  check_v4si (v, w);
}

void v4si_vspltisw_neg_addself ()
{
  v4si v = { -24, -24, -24, -24 };
  vspltisw (w, -24);
  check_v4si (v, w);
}



int main ()
{
  v16qi_vspltisb ();
  v16qi_vspltisb_neg ();
  v16qi_vspltisb_addself ();
  v16qi_vspltisb_neg_addself ();
  v16qi_vspltish ();
  v16qi_vspltish_addself ();
  v16qi_vspltish_neg ();
  v16qi_vspltisw ();
  v16qi_vspltisw_addself ();
  v16qi_vspltisw_neg ();

  v8hi_vspltisb ();
  v8hi_vspltisb_addself ();
  v8hi_vspltisb_neg ();
  v8hi_vspltish ();
  v8hi_vspltish_neg ();
  v8hi_vspltish_addself ();
  v8hi_vspltish_neg_addself ();
  v8hi_vspltisw ();
  v8hi_vspltisw_addself ();
  v8hi_vspltisw_neg ();

  v4si_vspltisb ();
  v4si_vspltisb_addself ();
  v4si_vspltisb_neg ();
  v4si_vspltish ();
  v4si_vspltish_addself ();
  v4si_vspltish_neg ();
  v4si_vspltisw ();
  v4si_vspltisw_neg ();
  v4si_vspltisw_addself ();
  v4si_vspltisw_neg_addself ();
  return 0;
}
