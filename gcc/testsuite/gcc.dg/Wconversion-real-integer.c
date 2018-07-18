/* Test for diagnostics for Wconversion between floating-point and
   integers.  */

/* { dg-do compile } */
/* { dg-skip-if "doubles are floats,ints are 16bits" { "avr-*-*" } } */
/* { dg-options "-std=c99 -Wconversion" } */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target double64plus } */
#include <limits.h>

void fsi (signed int x);
void fui (unsigned int x);
void ffloat (float x);
void fdouble (double x);

float  vfloat;
double vdouble;

void h (void)
{
  unsigned int ui = 3;
  int   si = 3;
  unsigned char uc = 3;
  signed char sc = 3;
  float  f = 3;
  double d = 3;

  fsi (3.1f); /* { dg-warning "conversion" } */
  si = 3.1f; /* { dg-warning "conversion" } */
  fsi (3.1);  /* { dg-warning "conversion" } */
  si = 3.1;  /* { dg-warning "conversion" } */
  fsi (d);    /* { dg-warning "conversion" } */
  si = d;    /* { dg-warning "conversion" } */
  fui (-1.0); /* { dg-warning "overflow" } */
  ui = -1.0;   /* { dg-warning "overflow" } */
  ffloat (INT_MAX);  /* { dg-warning "conversion" } */
  vfloat = INT_MAX;  /* { dg-warning "conversion" } */
  ffloat (16777217); /* { dg-warning "conversion" } */
  vfloat = 16777217; /* { dg-warning "conversion" } */
  ffloat (si); /* { dg-warning "conversion" } */
  vfloat = si; /* { dg-warning "conversion" } */
  ffloat (ui); /* { dg-warning "conversion" } */
  vfloat = ui; /* { dg-warning "conversion" } */

  fsi (3);
  si = 3;
  fsi (3.0f);
  si = 3.0f;
  fsi (3.0);
  si = 3.0;
  fsi (16777217.0f);
  si = 16777217.0f;
  fsi ((int) 3.1);
  si = (int) 3.1;
  ffloat (3U);
  vfloat = 3U;
  ffloat (3);
  vfloat = 3;
  ffloat (INT_MIN);
  vfloat = INT_MIN;
  ffloat (uc);
  vfloat = uc;
  ffloat (sc);
  vfloat = sc;

  fdouble (UINT_MAX);
  vdouble = UINT_MAX;
  fdouble (ui);
  vdouble = ui;
  fdouble (si);
  vdouble = si;
}


void fss (signed short x);
void fus (unsigned short x);
void fsc (signed char x);
void fuc (unsigned char x);

void h2 (void)
{
  unsigned short int us;
  short int   ss;
  unsigned char uc;
  signed char sc;
  
  fss (4294967294.0); /* { dg-warning "conversion" } */
  ss = 4294967294.0; /* { dg-warning "conversion" } */
  fss (-4294967294.0);  /* { dg-warning "conversion" } */
  ss = -4294967294.0;  /* { dg-warning "conversion" } */
  fus (4294967294.0); /* { dg-warning "conversion" } */
  us = 4294967294.0; /* { dg-warning "conversion" } */
  fus (-4294967294.0);  /* { dg-warning "conversion" } */
  us = -4294967294.0;  /* { dg-warning "conversion" } */

  fsc (500.0); /* { dg-warning "conversion" } */
  sc = 500.0; /* { dg-warning "conversion" } */
  fsc (-500.0);  /* { dg-warning "conversion" } */
  sc = -500.0;  /* { dg-warning "conversion" } */
  fuc (500.0); /* { dg-warning "conversion" } */
  uc = 500.0; /* { dg-warning "conversion" } */
  fuc (-500.0);  /* { dg-warning "conversion" } */
  uc = -500.0;  /* { dg-warning "conversion" } */

  fss (500.0);
  ss = 500.0;
  fss (-500.0);
  ss = -500.0;
  fus (500.0); 
  us = 500.0; 
  fus (-500.0);   /* { dg-warning "conversion" } */
  us = -500.0;    /* { dg-warning "conversion" } */
}
