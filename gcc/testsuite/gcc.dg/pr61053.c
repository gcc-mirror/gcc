/* PR c/61053 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Alignas (char) char cc;
_Alignas (short int) char cs;
_Alignas (int) char ci;
_Alignas (long int) char cl;
_Alignas (long long int) char cll;
_Alignas (float) char cf;
_Alignas (double) char cd;
_Alignas (long double) char cld;

_Alignas (char) short int sc; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) short int ss;
_Alignas (int) short int si;
_Alignas (long int) short int sl;
_Alignas (long long int) short int sll;
_Alignas (float) short int sf;
_Alignas (double) short int sd;
_Alignas (long double) short int sld;

_Alignas (char) int ic; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) int is; /* { dg-error "cannot reduce alignment" } */
_Alignas (int) int ii;
_Alignas (long int) int il;
_Alignas (long long int) int ill;
_Alignas (float) int if_;
_Alignas (double) int id;
_Alignas (long double) int ild;

_Alignas (char) long int lic; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) long int lis; /* { dg-error "cannot reduce alignment" } */
_Alignas (int) long int lii; /* { dg-error "cannot reduce alignment" "" { target { ! { ilp32 } } } } */
_Alignas (long int) long int lil;
_Alignas (long long int) long int lill;
_Alignas (float) long int lif; /* { dg-error "cannot reduce alignment" "" { target { ! { ilp32 } } } } */
_Alignas (double) long int lid;
_Alignas (long double) long int lild;

_Alignas (char) long long int llic; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) long long int llis; /* { dg-error "cannot reduce alignment" } */
_Alignas (int) long long int llii; /* { dg-error "cannot reduce alignment" "" { target { ! { ia32 } } } } */
_Alignas (long int) long long int llil; /* { dg-error "cannot reduce alignment" "" { target { x32 } } } */
_Alignas (long long int) long long int llill;
_Alignas (float) long long int llif; /* { dg-error "cannot reduce alignment" "" { target { ! { ia32 } } } } */
_Alignas (double) long long int llid;
_Alignas (long double) long long int llild;

_Alignas (char) float fc; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) float fs; /* { dg-error "cannot reduce alignment" } */
_Alignas (int) float fi;
_Alignas (long int) float fl;
_Alignas (long long int) float fll;
_Alignas (float) float ff;
_Alignas (double) float fd;
_Alignas (long double) float fld;

_Alignas (char) double dc; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) double ds; /* { dg-error "cannot reduce alignment" } */
_Alignas (int) double di; /* { dg-error "cannot reduce alignment" "" { target { ! { ia32 } } } } */
_Alignas (long int) double dl; /* { dg-error "cannot reduce alignment" "" { target { x32 } } } */
_Alignas (long long int) double dll;
_Alignas (float) double df; /* { dg-error "cannot reduce alignment" "" { target { ! { ia32 } } } } */
_Alignas (double) double dd;
_Alignas (long double) double dld;

_Alignas (char) long double ldc; /* { dg-error "cannot reduce alignment" } */
_Alignas (short int) long double lds; /* { dg-error "cannot reduce alignment" } */

#if __SIZEOF_LONG_DOUBLE__ == 12
/* Get around PR testsuite/69573 - FAIL: gcc.dg/pr61053.c (test for excess
   errors) on targets such as x86_64-apple-darwin15.3.0 where long double
   is 16 bytes wide even in LP32.  */
#  define X(T)   short
#else
#  define X(T)   T
#endif

_Alignas (X (int)) long double ldi; /* { dg-error "cannot reduce alignment" } */
_Alignas (X (long int)) long double ldl; /* { dg-error "cannot reduce alignment" } */
_Alignas (X (long long int)) long double ldll; /* { dg-error "cannot reduce alignment" } */
_Alignas (X (float)) long double ldf; /* { dg-error "cannot reduce alignment" } */
_Alignas (X (double)) long double ldd; /* { dg-error "cannot reduce alignment" } */
_Alignas (long double) long double ldld;
