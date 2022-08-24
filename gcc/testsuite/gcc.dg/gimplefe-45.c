/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */

/* This used to ICE when simplifying (A & C) != 0 ? D : 0
   for pointer types. */

int *__GIMPLE ()
p (int n)
{
  int *_2;
  int *_t;
  int *_t1;
  _Bool _3;
  _t = (int*)8;
  _t1 = 0;
  n = n & 2;
  _3 = n != 0;
  _2 = _3 ? _t : _t1;
  return _2;
}

