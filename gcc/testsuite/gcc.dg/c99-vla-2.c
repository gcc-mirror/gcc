/* Test composite type of VLA and fixed-size array: should be the
   fixed-size type.  Bug 22192.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* Test that the given expression (of pointer-to-array type) points to
   the right sort of array.  */
#define TEST_FIXED_SIZE(a) do { static int x[sizeof(*(a))]; (void)x; } while (0)
#define TEST_VLA(a) do { (void)sizeof(*(a)); (void)(1 ? (a) : (__typeof__(**(a)) (*)[1])0); (void)(1 ? (a) : (__typeof__(**(a)) (*)[2])0); } while (0)
#define TEST_INCOMPLETE(a) do { __typeof__(*(a)) x = { 0 }; (void)x; (void)(1 ? a : (__typeof__(**(a)) (*)[1])0); (void)(1 ? a : (__typeof__(**(a)) (*)[2])0); } while (0)

#define TEST_COMP_FIX(a, b) TEST_FIXED_SIZE(i ? a : b)
#define TEST_COMP_VLA(a, b) TEST_VLA(i ? a : b)
#define TEST_COMP_INC(a, b) TEST_INCOMPLETE(i ? a : b)

void
foo (int i, int j)
{
  typedef int I;
  int (*pf)[2];
  int (*pv)[i];
  int (*pi)[];
  I (*pfI)[2];
  I (*pvI)[i];
  I (*piI)[];
  TEST_COMP_FIX(pf, pf);
  TEST_COMP_FIX(pf, pv);
  TEST_COMP_FIX(pf, pi);
  TEST_COMP_FIX(pf, pfI);
  TEST_COMP_FIX(pf, pvI);
  TEST_COMP_FIX(pf, piI);
  TEST_COMP_FIX(pv, pf);
  TEST_COMP_VLA(pv, pv);
  TEST_COMP_VLA(pv, pi);
  TEST_COMP_FIX(pv, pfI);
  TEST_COMP_VLA(pv, pvI);
  TEST_COMP_VLA(pv, piI);
  TEST_COMP_FIX(pi, pf);
  TEST_COMP_VLA(pi, pv);
  TEST_COMP_INC(pi, pi);
  TEST_COMP_FIX(pi, pfI);
  TEST_COMP_VLA(pi, pvI);
  TEST_COMP_INC(pi, piI);
  TEST_COMP_FIX(pfI, pf);
  TEST_COMP_FIX(pfI, pv);
  TEST_COMP_FIX(pfI, pi);
  TEST_COMP_FIX(pfI, pfI);
  TEST_COMP_FIX(pfI, pvI);
  TEST_COMP_FIX(pfI, piI);
  TEST_COMP_FIX(pvI, pf);
  TEST_COMP_VLA(pvI, pv);
  TEST_COMP_VLA(pvI, pi);
  TEST_COMP_FIX(pvI, pfI);
  TEST_COMP_VLA(pvI, pvI);
  TEST_COMP_VLA(pvI, piI);
  TEST_COMP_FIX(piI, pf);
  TEST_COMP_VLA(piI, pv);
  TEST_COMP_INC(piI, pi);
  TEST_COMP_FIX(piI, pfI);
  TEST_COMP_VLA(piI, pvI);
  TEST_COMP_INC(piI, piI);
  typedef int (*Ti)[i];
  typedef int (*Tj)[j];
  Ti (*qf)[2];
  Ti (*qv)[i];
  Ti (*qi)[];
  Tj (*rf)[2];
  Tj (*rv)[j];
  Tj (*ri)[];
  TEST_COMP_FIX(qf, qf);
  TEST_COMP_FIX(qf, qv);
  TEST_COMP_FIX(qf, qi);
  TEST_COMP_FIX(qf, rf);
  TEST_COMP_FIX(qf, rv);
  TEST_COMP_FIX(qf, ri);
  TEST_COMP_FIX(qv, qf);
  TEST_COMP_VLA(qv, qv);
  TEST_COMP_VLA(qv, qi);
  TEST_COMP_FIX(qv, rf);
  TEST_COMP_VLA(qv, rv);
  TEST_COMP_VLA(qv, ri);
  TEST_COMP_FIX(qi, qf);
  TEST_COMP_VLA(qi, qv);
  TEST_COMP_INC(qi, qi);
  TEST_COMP_FIX(qi, rf);
  TEST_COMP_VLA(qi, rv);
  TEST_COMP_INC(qi, ri);
  TEST_COMP_FIX(rf, qf);
  TEST_COMP_FIX(rf, qv);
  TEST_COMP_FIX(rf, qi);
  TEST_COMP_FIX(rf, rf);
  TEST_COMP_FIX(rf, rv);
  TEST_COMP_FIX(rf, ri);
  TEST_COMP_FIX(rv, qf);
  TEST_COMP_VLA(rv, qv);
  TEST_COMP_VLA(rv, qi);
  TEST_COMP_FIX(rv, rf);
  TEST_COMP_VLA(rv, rv);
  TEST_COMP_VLA(rv, ri);
  TEST_COMP_FIX(ri, qf);
  TEST_COMP_VLA(ri, qv);
  TEST_COMP_INC(ri, qi);
  TEST_COMP_FIX(ri, rf);
  TEST_COMP_VLA(ri, rv);
  TEST_COMP_INC(ri, ri);
}
