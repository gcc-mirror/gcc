/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Test scalar passing and return values involving decimal floating
   point types.  */

extern void abort (void);

/* A handful of functions that return their Nth _Decimal32
   argument.  */

_Decimal32
arg0_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg0;
}

_Decimal32
arg1_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg1;
}
	
_Decimal32
arg2_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg2;
}
	
_Decimal32
arg3_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg3;
}
	
_Decimal32
arg4_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg4;
}
	
_Decimal32
arg5_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg5;
}
	

/* A handful of functions that return their Nth _Decimal64
   argument.  */

_Decimal64
arg0_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg0;
}
	
_Decimal64
arg1_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg1;
}
	
_Decimal64
arg2_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg2;
}
	
_Decimal64
arg3_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg3;
}
	
_Decimal64
arg4_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg4;
}
	
_Decimal64
arg5_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg5;
}
	

/* A handful of functions that return their Nth _Decimal128
   argument.  */

_Decimal128
arg0_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg0;
}
	
_Decimal128
arg1_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg1;
}
	
_Decimal128
arg2_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg2;
}
	
_Decimal128
arg3_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg3;
}
	
_Decimal128
arg4_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg4;
}
	
_Decimal128
arg5_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg5;
}



int
main ()
{
  /* _Decimal32 variants.  */
  if (arg0_32 (0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 0.0df)
    abort ();
  if (arg1_32 (0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 1.0df)
    abort ();
  if (arg2_32 (0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 2.0df)
    abort ();
  if (arg3_32 (0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 3.0df)
    abort ();
  if (arg4_32 (0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 4.0df)
    abort ();
  if (arg5_32 (0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 5.0df)
    abort ();

  /* _Decimal64 variants.  */
  if (arg0_64 (0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 0.0dd)
    abort ();
  if (arg1_64 (0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 1.0dd)
    abort ();
  if (arg2_64 (0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 2.0dd)
    abort ();
  if (arg3_64 (0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 3.0dd)
    abort ();
  if (arg4_64 (0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 4.0dd)
    abort ();
  if (arg5_64 (0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 5.0dd)
    abort ();

  /* _Decimal128 variants.  */
  if (arg0_128 (0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 0.0dl)
    abort ();
  if (arg1_128 (0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 1.0dl)
    abort ();
  if (arg2_128 (0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 2.0dl)
    abort ();
  if (arg3_128 (0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 3.0dl)
    abort ();
  if (arg4_128 (0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 4.0dl)
    abort ();
  if (arg5_128 (0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 5.0dl)
    abort ();

  return 0;
}
