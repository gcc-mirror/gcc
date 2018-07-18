/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

extern int convert_to_int (float f);
extern int convert_to_int (double d);
extern int convert_to_int (long double ld);

class S1
{
  S1 ();
  int convert_to_int (float f);
  int convert_to_int (double d);
  int convert_to_int (long double ld);
};

class S2
{
  S2 ();
  virtual int convert_to_int (float f);
  virtual int convert_to_int (double d);
  virtual int convert_to_int (long double ld);
};

/* { dg-final { cleanup-ada-spec } } */
