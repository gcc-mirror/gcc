/* Copyright (C) 2021 Free Software Foundation.

   Check that constant folding of built-in fmod functions doesn't
   break anything and produces the expected results.

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

extern double fmod(double,double);
extern float fmodf(float,float);
extern long double fmodl(long double,long double);

int main()
{
  if (fmod (6.5, 2.3) < 1.8999 || fmod (6.5, 2.3) > 1.9001)
    link_error ();
  if (fmod (-6.5, 2.3) < -1.9001 || fmod (-6.5, 2.3) > -1.8999)
    link_error ();
  if (fmod (6.5, -2.3) < 1.8999 || fmod (6.5, -2.3) > 1.9001)
    link_error ();
  if (fmod (-6.5, -2.3) < -1.9001 || fmod (-6.5, -2.3) > -1.8999)
    link_error ();

  if (fmodf (6.5f, 2.3f) < 1.8999f || fmodf (6.5f, 2.3f) > 1.9001f)
    link_error ();
  if (fmodf (-6.5f, 2.3f) < -1.9001f || fmodf (-6.5f, 2.3f) > -1.8999f)
    link_error ();
  if (fmodf (6.5f, -2.3f) < 1.8999f || fmodf (6.5f, -2.3f) > 1.9001f)
    link_error ();
  if (fmodf (-6.5f, -2.3f) < -1.9001f || fmodf (-6.5f, -2.3f) > -1.8999f)
    link_error ();

  if (fmodl (6.5l, 2.3l) < 1.8999l || fmod (6.5l, 2.3l) > 1.9001l)
    link_error ();
  if (fmodl (-6.5l, 2.3l) < -1.9001l || fmod (-6.5l, 2.3l) > -1.8999l)
    link_error ();
  if (fmodl (6.5l, -2.3l) < 1.8999l || fmod (6.5l, -2.3l) > 1.9001l)
    link_error ();
  if (fmodl (-6.5l, -2.3l) < -1.9001l || fmod (-6.5l, -2.3l) > -1.8999l)
    link_error ();

  return 0;
}

