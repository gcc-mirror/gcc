/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
__extension__ typedef unsigned long long int uint64_t;
typedef int intptr_t;
typedef struct BigStruct{
  uint8_t a;
  int8_t b;
  uint16_t c;
  int16_t d;
  uint32_t e;
  int32_t f;
  uint64_t g;
  int64_t h;
  float i;
  double j;
  long double k;
  char* l;
  uint8_t m;
  int8_t n;
  uint16_t o;
  int16_t p;
  uint32_t q;
  int32_t r;
  uint64_t s;
  int64_t t;
  float u;
  double v;
  long double w;
  char* x;
  uint8_t y;
  int8_t z;
  uint16_t aa;
  int16_t bb;
  uint32_t cc;
  int32_t dd;
  uint64_t ee;
  int64_t ff;
  float gg;
  double hh;
  long double ii;
  char* jj;
  uint8_t kk;
  int8_t ll;
  uint16_t mm;
  int16_t nn;
  uint32_t oo;
  int32_t pp;
  uint64_t qq;
  int64_t rr;
  float ss;
  double tt;
  long double uu;
  char* vv;
  uint8_t ww;
  int8_t xx;
}  BigStruct;

extern void foobar(const char *fmt, ...);

void
test_large_fn (uint8_t ui8_1, int8_t si8_1, uint16_t ui16_1, int16_t si16_1,
	       uint32_t ui32_1, int32_t si32_1, uint64_t ui64_1, int64_t si64_1,
	       float f_1, double d_1, long double ld_1, char* p_1,
	       uint8_t ui8_2, int8_t si8_2, uint16_t ui16_2, int16_t si16_2,
	       uint32_t ui32_2, int32_t si32_2, uint64_t ui64_2, int64_t si64_2,
	       float f_2, double d_2, long double ld_2, char* p_2,
	       uint8_t ui8_3, int8_t si8_3, uint16_t ui16_3, int16_t si16_3,
	       uint32_t ui32_3, int32_t si32_3, uint64_t ui64_3, int64_t si64_3,
	       float f_3, double d_3, long double ld_3, char* p_3,
	       uint8_t ui8_4, int8_t si8_4, uint16_t ui16_4, int16_t si16_4,
	       uint32_t ui32_4, int32_t si32_4, uint64_t ui64_4, int64_t si64_4,
	       float f_4, double d_4, long double ld_4, char* p_4,
	       uint8_t ui8_5, int8_t si8_5)
{
    BigStruct retVal =
      {
	ui8_1 + 1, si8_1 + 1, ui16_1 + 1, si16_1 + 1,
	ui32_1 + 1, si32_1 + 1, ui64_1 + 1, si64_1 + 1,
	f_1 + 1, d_1 + 1, ld_1 + 1, (char*)((intptr_t)p_1 + 1),
	ui8_2 + 2, si8_2 + 2, ui16_2 + 2, si16_2 + 2,
	ui32_2 + 2, si32_2 + 2, ui64_2 + 2, si64_2 + 2,
	f_2 + 2, d_2 + 2, ld_2 + 2, (char*)((intptr_t)p_2 + 2),
	ui8_3 + 3, si8_3 + 3, ui16_3 + 3, si16_3 + 3,
	ui32_3 + 3, si32_3 + 3, ui64_3 + 3, si64_3 + 3,
	f_3 + 3, d_3 + 3, ld_3 + 3, (char*)((intptr_t)p_3 + 3),
	ui8_4 + 4, si8_4 + 4, ui16_4 + 4, si16_4 + 4,
	ui32_4 + 4, si32_4 + 4, ui64_4 + 4, si64_4 + 4,
	f_4 + 4, d_4 + 4, ld_4 + 4, (char*)((intptr_t)p_4 + 4),
	ui8_5 + 5, si8_5 + 5
      };

    foobar ("%" "u" " %" "d" " %hu %hd %u %d %" "ll" "u" " %" "ll" "d"
	      " %.0f %.0f %.0Lf %#lx " "%" "u" " %" "d" " %hu %hd %u %d %"
	      "ll" "u" " %" "ll" "d" " %.0f %.0f %.0Lf %#lx " "%" "u"
	      " %" "d" " %hu %hd %u %d %" "ll" "u" " %" "ll" "d"
	      " %.0f %.0f %.0Lf %#lx " "%" "u" " %" "d" " %hu %hd %u %d %"
	      "ll" "u" " %" "ll" "d" " %.0f %.0f %.0Lf %#lx %" "u" " %"
	      "d" ": " "%" "u" " %" "d" " %hu %hd %u %d %" "ll" "u" " %"
	      "ll" "d" " %.0f %.0f %.0Lf %#lx " "%" "u" " %" "d"
	      " %hu %hd %u %d %" "ll" "u" " %" "ll" "d" " %.0f %.0f %.0Lf %#lx "
	      "%" "u" " %" "d" " %hu %hd %u %d %" "ll" "u" " %" "ll" "d"
	      " %.0f %.0f %.0Lf %#lx " "%" "u" " %" "d" " %hu %hd %u %d %"
	    "ll" "u" " %" "ll" "d" " %.0f %.0f %.0Lf %#lx %" "u" " %" "d" "\n",
	    ui8_1, si8_1, ui16_1, si16_1, ui32_1, si32_1, ui64_1, si64_1,
	    f_1, d_1, ld_1, (unsigned long)p_1, ui8_2, si8_2, ui16_2, si16_2,
	    ui32_2, si32_2, ui64_2, si64_2, f_2, d_2, ld_2, (unsigned long)p_2,
	    ui8_3, si8_3, ui16_3, si16_3, ui32_3, si32_3, ui64_3, si64_3, f_3,
	    d_3, ld_3, (unsigned long)p_3, ui8_4, si8_4, ui16_4, si16_4, ui32_4,
	    si32_4, ui64_4, si64_4, f_4, d_4, ld_4, (unsigned long)p_4, ui8_5,
	    si8_5, retVal.a, retVal.b, retVal.c, retVal.d, retVal.e, retVal.f,
	    retVal.g, retVal.h, retVal.i, retVal.j, retVal.k,
	    (unsigned long)retVal.l, retVal.m, retVal.n, retVal.o, retVal.p,
	    retVal.q, retVal.r, retVal.s, retVal.t, retVal.u, retVal.v,
	    retVal.w, (unsigned long)retVal.x, retVal.y, retVal.z, retVal.aa,
	    retVal.bb, retVal.cc, retVal.dd, retVal.ee, retVal.ff, retVal.gg,
	    retVal.hh, retVal.ii, (unsigned long)retVal.jj, retVal.kk,
	    retVal.ll, retVal.mm, retVal.nn, retVal.oo, retVal.pp, retVal.qq,
	    retVal.rr, retVal.ss, retVal.tt, retVal.uu, (unsigned long)retVal.vv,
	    retVal.ww, retVal.xx);
}
