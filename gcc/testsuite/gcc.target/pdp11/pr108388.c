/* { dg-do compile } */
/* { dg-options "-O2 -mlra" } */

typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
extern DItype __mulvdi3 (DItype, DItype);
struct DWstruct {SItype high, low;};

typedef union {
  struct DWstruct s;
  DItype ll;
} DWunion;

DItype __mulvdi3 (DItype u, DItype v) {
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};
  
  if (__builtin_expect (uu.s.high == uu.s.low >> ((4 * 8) - 1), 1)) {
    if (__builtin_expect (vv.s.high == vv.s.low >> ((4 * 8) - 1), 1)) {
      return (DItype) uu.s.low * (DItype) vv.s.low;
    } else {
      DWunion w0 = {.ll = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.low};
      DWunion w1 = {.ll = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.high};
      
      if (vv.s.high < 0)
	w1.s.high -= uu.s.low;
      if (uu.s.low < 0)
	w1.ll -= vv.ll;
      w1.ll += (USItype) w0.s.high;
      if (__builtin_expect (w1.s.high == w1.s.low >> ((4 * 8) - 1), 1))	{
	w0.s.high = w1.s.low;
	return w0.ll;
      }
    }
  } else {
    if (__builtin_expect (vv.s.high == vv.s.low >> ((4 * 8) - 1), 1)) {
      DWunion w0 = {.ll = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.low};
      DWunion w1 = {.ll = (UDItype) (USItype) uu.s.high * (UDItype) (USItype) vv.s.low};

      if (uu.s.high < 0)
	w1.s.high -= vv.s.low;
      if (vv.s.low < 0)
	w1.ll -= uu.ll;
      w1.ll += (USItype) w0.s.high;
      if (__builtin_expect (w1.s.high == w1.s.low >> ((4 * 8) - 1), 1))	{
	w0.s.high = w1.s.low;
	return w0.ll;
      }
    } else {
      if (uu.s.high >= 0) {
	if (vv.s.high >= 0) {
	  if (uu.s.high == 0 && vv.s.high == 0) {
	    const DItype w = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.low;
	    if (__builtin_expect (w >= 0, 1))
	      return w;
	  }
	} else {
	  if (uu.s.high == 0 && vv.s.high == (SItype) -1) {
	    DWunion ww = {.ll = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.low};
	    ww.s.high -= uu.s.low;
	    if (__builtin_expect (ww.s.high < 0, 1))
	      return ww.ll;
	  }
	}
      } else {
	if (vv.s.high >= 0) {
	  if (uu.s.high == (SItype) -1 && vv.s.high == 0) {
	    DWunion ww = {.ll = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.low};
	    
	    ww.s.high -= vv.s.low;
	    if (__builtin_expect (ww.s.high < 0, 1))
	      return ww.ll;
	  }
	} else {
	  if ((uu.s.high & vv.s.high) == (SItype) -1 && (uu.s.low | vv.s.low) != 0) {
	      DWunion ww = {.ll = (UDItype) (USItype) uu.s.low * (UDItype) (USItype) vv.s.low};
	      
	      ww.s.high -= uu.s.low;
	      ww.s.high -= vv.s.low;
	      if (__builtin_expect (ww.s.high >= 0, 1))
		return ww.ll;
	  }
	}
      }
    }
  }
  __builtin_trap ();
}
