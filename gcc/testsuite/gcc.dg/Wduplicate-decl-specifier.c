/* PR43651 */
/* { dg-do compile } */
/* { dg-options "-Wduplicate-decl-specifier" } */

typedef const int CT1;
#define CT2 const int
typedef volatile int VT1;
#define VT2 volatile int
typedef char *restrict RT1;
#define RT2 char *restrict

void
foo (void)
{
  const CT1 x1;
  const CT2 x2;
  CT1 const x3;
  CT2 const x4;
  const int const x5; /* { dg-warning "duplicate .const." } */
  const int *const x6;
  volatile VT1 y1;
  volatile VT2 y2;
  VT1 volatile y3;
  VT2 volatile y4;
  volatile int volatile y5; /* { dg-warning "duplicate .volatile." } */
  volatile int *volatile y6;
  RT1 restrict r1;
  RT2 restrict r2;
  restrict RT1 r3;
  /* "restrict RT2" is invalid */
  char *restrict restrict r4; /* { dg-warning "duplicate .restrict." } */
  char *restrict *restrict r5;
}

void c1(const CT1 t) { }
void c2(const CT2 t) { }
void c3(CT1 const t) { }
void c4(CT2 const t) { }
void c5(const int const t) { }  /* { dg-warning "duplicate .const." } */
void v1(volatile VT1 t) { }
void v2(volatile VT2 t) { }
void v3(VT1 volatile t) { }
void v4(VT2 volatile t) { }
void v5(volatile int volatile t) { } /* { dg-warning "duplicate .volatile." } */
void r1(restrict RT1 t) { }
void r2(RT1 restrict t) { }
void r3(RT2 restrict t) { }
void r4(char *restrict restrict t) { }  /* { dg-warning "duplicate .restrict." } */

typedef const CT1 CCT1;
typedef const CT2 CCT2;
typedef CT1 const CT1C;
typedef CT2 const CT2C;
typedef const int const CIC;    /* { dg-warning "duplicate .const." } */
typedef volatile VT1 VVT1;
typedef volatile VT2 VVT2;
typedef VT1 volatile VT1V;
typedef VT2 volatile VT2V;
typedef volatile int volatile VIV; /* { dg-warning "duplicate .volatile." } */
typedef RT1 restrict RT1R;
typedef RT2 restrict RT2R;
typedef restrict RT1 RRT1;
typedef int *restrict restrict IRR; /* { dg-warning "duplicate .restrict." } */
