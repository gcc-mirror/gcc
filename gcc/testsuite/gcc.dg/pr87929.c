/* { dg-do compile } */
/* { dg-options "-fexceptions -fnon-call-exceptions -fsignaling-nans" } */

#define complex __complex__
#define _Complex_I (1.0iF)

extern void f2c_4d__( complex float *, complex float *);
extern void abort (void);

void f2c_4c__(void)
{
  complex float x,ret_val;
  x = 1234 + 5678 * _Complex_I;
  f2c_4d__(&ret_val,&x);
  if ( x != ret_val ) abort();
}
