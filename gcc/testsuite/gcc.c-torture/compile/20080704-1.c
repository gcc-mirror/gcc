/* This code used to crash fold_convert due to PRE
   wanting to fold_convert from a REAL_TYPE to an INTEGER_TYPE.  */
/* { dg-additional-options "-fpermissive" } */
typedef unsigned int uint32_t;
union double_union
{
  double d;
  uint32_t i[2];
};
struct _Jv_reent
{
  int _errno;
};
_Jv_strtod_r (struct _Jv_reent *ptr, char **se)
{
  int bb2, sign;
  double aadj, aadj1, adj;
  unsigned long y, z;
  union double_union rv, *bs = ((void *) 0), *delta = ((void *) 0);
  {
  ovfl:ptr->_errno = 34;
    {
      (((uint32_t) 0xfffffL) | ((uint32_t) 0x100000L) * (1024 + 1023 - 1));
    }
    if ((aadj = _Jv_ratio (delta, bs)) <= 2.)
      {
	{
	  if (aadj < 2. / 2)
	    aadj = 1. / 2;
	  aadj1 = -aadj;
	}
      }
    {
      (rv.i[1]) -= 53 * ((uint32_t) 0x100000L);
      adj = aadj1 * _Jv_ulp (rv.d);
      rv.d += adj;
      if (((rv.i[1]) & ((uint32_t) 0x7ff00000L)) >=
	  ((uint32_t) 0x100000L) * (1024 + 1023 - 53))
	{
	  goto ovfl;
	}
    }
  }
}
