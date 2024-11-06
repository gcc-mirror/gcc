/* { dg-additional-options { -std=c99 } } */

typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));


typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

struct DWstruct {SItype low, high;};

typedef union
{
  struct DWstruct s;
  DItype ll;
} DWunion;


static inline __attribute__ ((__always_inline__))

UDItype
__udivmoddi4 (UDItype n, UDItype d, UDItype *rp)
{
  const DWunion nn = {.ll = n};
  const DWunion dd = {.ll = d};
  DWunion rr;
  USItype d0, d1, n0, n1, n2;
  USItype q0, q1;
  USItype b, bm;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;

  if (d1 == 0)
    {
      if (d0 > n1)
 {


   ((bm) = __builtin_clzl (d0));

   if (bm != 0)
     {



       d0 = d0 << bm;
       n1 = (n1 << bm) | (n0 >> ((4 * 8) - bm));
       n0 = n0 << bm;
     }

   do { USItype __d1, __d0, __q1, __q0; USItype __r1, __r0, __m; __d1 = ((USItype) (d0) >> ((4 * 8) / 2)); __d0 = ((USItype) (d0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); __r1 = (n1) % __d1; __q1 = (n1) / __d1; __m = (USItype) __q1 * __d0; __r1 = __r1 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n0) >> ((4 * 8) / 2)); if (__r1 < __m) { __q1--, __r1 += (d0); if (__r1 >= (d0)) if (__r1 < __m) __q1--, __r1 += (d0); } __r1 -= __m; __r0 = __r1 % __d1; __q0 = __r1 / __d1; __m = (USItype) __q0 * __d0; __r0 = __r0 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); if (__r0 < __m) { __q0--, __r0 += (d0); if (__r0 >= (d0)) if (__r0 < __m) __q0--, __r0 += (d0); } __r0 -= __m; (q0) = (USItype) __q1 * ((USItype) 1 << ((4 * 8) / 2)) | __q0; (n0) = __r0; } while (0);
   q1 = 0;


 }
      else
 {


   if (d0 == 0)
     d0 = 1 / d0;

   ((bm) = __builtin_clzl (d0));

   if (bm == 0)
     {







       n1 -= d0;
       q1 = 1;
     }
   else
     {


       b = (4 * 8) - bm;

       d0 = d0 << bm;
       n2 = n1 >> b;
       n1 = (n1 << bm) | (n0 >> b);
       n0 = n0 << bm;

       do { USItype __d1, __d0, __q1, __q0; USItype __r1, __r0, __m; __d1 = ((USItype) (d0) >> ((4 * 8) / 2)); __d0 = ((USItype) (d0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); __r1 = (n2) % __d1; __q1 = (n2) / __d1; __m = (USItype) __q1 * __d0; __r1 = __r1 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n1) >> ((4 * 8) / 2)); if (__r1 < __m) { __q1--, __r1 += (d0); if (__r1 >= (d0)) if (__r1 < __m) __q1--, __r1 += (d0); } __r1 -= __m; __r0 = __r1 % __d1; __q0 = __r1 / __d1; __m = (USItype) __q0 * __d0; __r0 = __r0 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n1) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); if (__r0 < __m) { __q0--, __r0 += (d0); if (__r0 >= (d0)) if (__r0 < __m) __q0--, __r0 += (d0); } __r0 -= __m; (q1) = (USItype) __q1 * ((USItype) 1 << ((4 * 8) / 2)) | __q0; (n1) = __r0; } while (0);
     }



   do { USItype __d1, __d0, __q1, __q0; USItype __r1, __r0, __m; __d1 = ((USItype) (d0) >> ((4 * 8) / 2)); __d0 = ((USItype) (d0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); __r1 = (n1) % __d1; __q1 = (n1) / __d1; __m = (USItype) __q1 * __d0; __r1 = __r1 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n0) >> ((4 * 8) / 2)); if (__r1 < __m) { __q1--, __r1 += (d0); if (__r1 >= (d0)) if (__r1 < __m) __q1--, __r1 += (d0); } __r1 -= __m; __r0 = __r1 % __d1; __q0 = __r1 / __d1; __m = (USItype) __q0 * __d0; __r0 = __r0 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); if (__r0 < __m) { __q0--, __r0 += (d0); if (__r0 >= (d0)) if (__r0 < __m) __q0--, __r0 += (d0); } __r0 -= __m; (q0) = (USItype) __q1 * ((USItype) 1 << ((4 * 8) / 2)) | __q0; (n0) = __r0; } while (0);


 }

      if (rp != 0)
 {
   rr.s.low = n0 >> bm;
   rr.s.high = 0;
   *rp = rr.ll;
 }
    }


  else
    {
      if (d1 > n1)
 {


   q0 = 0;
   q1 = 0;


   if (rp != 0)
     {
       rr.s.low = n0;
       rr.s.high = n1;
       *rp = rr.ll;
     }
 }
      else
 {


   ((bm) = __builtin_clzl (d1));
   if (bm == 0)
     {

       if (n1 > d1 || n0 >= d0)
  {
    q0 = 1;
    do { USItype __x; __x = (n0) - (d0); (n1) = (n1) - (d1) - (__x > (n0)); (n0) = __x; } while (0);
  }
       else
  q0 = 0;

       q1 = 0;

       if (rp != 0)
  {
    rr.s.low = n0;
    rr.s.high = n1;
    *rp = rr.ll;
  }
     }
   else
     {
       USItype m1, m0;


       b = (4 * 8) - bm;

       d1 = (d1 << bm) | (d0 >> b);
       d0 = d0 << bm;
       n2 = n1 >> b;
       n1 = (n1 << bm) | (n0 >> b);
       n0 = n0 << bm;

       do { USItype __d1, __d0, __q1, __q0; USItype __r1, __r0, __m; __d1 = ((USItype) (d1) >> ((4 * 8) / 2)); __d0 = ((USItype) (d1) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); __r1 = (n2) % __d1; __q1 = (n2) / __d1; __m = (USItype) __q1 * __d0; __r1 = __r1 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n1) >> ((4 * 8) / 2)); if (__r1 < __m) { __q1--, __r1 += (d1); if (__r1 >= (d1)) if (__r1 < __m) __q1--, __r1 += (d1); } __r1 -= __m; __r0 = __r1 % __d1; __q0 = __r1 / __d1; __m = (USItype) __q0 * __d0; __r0 = __r0 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n1) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); if (__r0 < __m) { __q0--, __r0 += (d1); if (__r0 >= (d1)) if (__r0 < __m) __q0--, __r0 += (d1); } __r0 -= __m; (q0) = (USItype) __q1 * ((USItype) 1 << ((4 * 8) / 2)) | __q0; (n1) = __r0; } while (0);
       do { USItype __x0, __x1, __x2, __x3; USItype __ul, __vl, __uh, __vh; __ul = ((USItype) (q0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); __uh = ((USItype) (q0) >> ((4 * 8) / 2)); __vl = ((USItype) (d0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); __vh = ((USItype) (d0) >> ((4 * 8) / 2)); __x0 = (USItype) __ul * __vl; __x1 = (USItype) __ul * __vh; __x2 = (USItype) __uh * __vl; __x3 = (USItype) __uh * __vh; __x1 += ((USItype) (__x0) >> ((4 * 8) / 2)); __x1 += __x2; if (__x1 < __x2) __x3 += ((USItype) 1 << ((4 * 8) / 2)); (m1) = __x3 + ((USItype) (__x1) >> ((4 * 8) / 2)); (m0) = ((USItype) (__x1) & (((USItype) 1 << ((4 * 8) / 2)) - 1)) * ((USItype) 1 << ((4 * 8) / 2)) + ((USItype) (__x0) & (((USItype) 1 << ((4 * 8) / 2)) - 1)); } while (0);

       if (m1 > n1 || (m1 == n1 && m0 > n0))
  {
    q0--;
    do { USItype __x; __x = (m0) - (d0); (m1) = (m1) - (d1) - (__x > (m0)); (m0) = __x; } while (0);
  }

       q1 = 0;


       if (rp != 0)
  {
    do { USItype __x; __x = (n0) - (m0); (n1) = (n1) - (m1) - (__x > (n0)); (n0) = __x; } while (0);
    rr.s.low = (n1 << b) | (n0 >> bm);
    rr.s.high = n1 >> bm;
    *rp = rr.ll;
  }
     }
 }
    }

  const DWunion ww = {{.low = q0, .high = q1}};
  return ww.ll;
}

DItype
__divmoddi4 (DItype u, DItype v, DItype *rp)
{
  SItype c1 = 0, c2 = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DItype w;
  DItype r;

  if (uu.s.high < 0)
    c1 = ~c1, c2 = ~c2,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    c1 = ~c1,
    vv.ll = -vv.ll;

  w = __udivmoddi4 (uu.ll, vv.ll, (UDItype*)&r);
  if (c1)
    w = -w;
  if (c2)
    r = -r;

  *rp = r;
  return w;
}
