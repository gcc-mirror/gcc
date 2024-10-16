/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

/* This formerly ICEd when trying to expand pow as a built-in with
   the wrong number of arguments.  */

extern double pow (double, double) __attribute__ ((__nothrow__ , __leaf__));

typedef struct {
  long long data;
  int tag;
} Object;

extern Object Make_Flonum (double);
extern Object P_Pow (Object, Object);

Object General_Function (Object x, Object y, double (*fun)()) {
  double d, ret;

  d = 1.0;

  if (y.tag >> 1)
    ret = (*fun) (d);
  else
    ret = (*fun) (d, 0.0);

  return Make_Flonum (ret);
}

Object P_Pow (Object x, Object y) { return General_Function (x, y, pow); }
