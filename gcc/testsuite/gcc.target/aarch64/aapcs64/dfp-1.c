/* Test loading DFP values from memory.  */

/* { dg-do compile { target aarch64*-*-* } } */

_Decimal32 var32 = 1.2df;

int foo32(_Decimal32 param32)
{
	return param32 == var32;
}

_Decimal64 var64 = 1.2df;

int foo64(_Decimal64 param64)
{
	return param64 == var64;
}

_Decimal128 var128 = 1.2df;

int foo128(_Decimal128 param128)
{
	return param128 == var128;
}
