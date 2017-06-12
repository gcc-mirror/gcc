/* PR c/80731 - poor -Woverflow warnings, missing detail
   { dg-do compile }
   { dg-options "-Wconversion -Woverflow -Wno-override-init -std=c99" }
   { dg-require-effective-target int32plus } */

#include <limits.h>

struct Types
{
  signed char sc;
  unsigned char uc;
  signed short ss;
  unsigned short us;
  signed int si;
  unsigned int ui;
  signed long sl;
  unsigned long ul;
  signed long long sll;
  unsigned long long ull;
};

const struct Types t1 = {
  /* According to 6.3.1.3 of C11:
     -2-  Otherwise, if the new type is unsigned, the value is converted
	  by repeatedly adding or subtracting one more than the maximum
	  value that can be represented in the new type until the value
	  is in the range of the new type.

     These conversions are diagnosed by -Wsign-conversion and -Wconversion,
     respectively, by mentioning "unsigned conversion" if the conversion
     results in sign change, and just "conversion" otherwise, as follows:  */

  .uc = SCHAR_MIN,          /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes value from .-128. to .128." } */
  .uc = -1,                 /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes value from .-1. to .255." } */

  .uc = UCHAR_MAX + 1,      /* { dg-warning "conversion from 'int' to 'unsigned char' changes value from .256. to .0." } */
  .uc = UCHAR_MAX * 2,      /* { dg-warning "conversion from 'int' to 'unsigned char' changes value from .510. to .254." } */

  /* According to 6.3.1.3 of C11:
     -3-  Otherwise, the new type is signed and the value cannot be
	  represented in it; either the result is implementation-defined
	  or an implementation-defined signal is raised.

     In GCC such conversions wrap and are diagnosed by mentioning "overflow"
     if the absolute value of the operand is in excess of the maximum of
     the destination of type, and "conversion" otherwise, as follows:  */

  .sc = SCHAR_MAX + 1,      /* { dg-warning "conversion from .int. to .signed char. changes value from .128. to .-128." } */
  .sc = SCHAR_MAX + 2,      /* { dg-warning "conversion from .int. to .signed char. changes value from .129. to .-127." } */
  .sc = SCHAR_MAX * 2,      /* { dg-warning "conversion from .int. to .signed char. changes value from .254. to .-2." } */
  .sc = SCHAR_MAX * 2 + 3,  /* { dg-warning "conversion from .int. to .signed char. changes value from .257. to .1." } */
  .sc = SCHAR_MAX * 3 + 3,  /* { dg-warning "conversion from .int. to .signed char. changes value from .384. to .-128." } */


  .ss = SHRT_MAX + 1,       /* { dg-warning "conversion from 'int' to 'short int' changes value from .32768. to .-32768." } */
  .us = USHRT_MAX + 1,      /* { dg-warning "unsigned conversion from .int. to .short unsigned int. changes value from .65536. to .0." } */

  .si = INT_MAX + 1LU,      /* { dg-warning "signed conversion from 'long unsigned int. to 'int' changes value from .2147483648. to .-2147483648." } */
  .ui = UINT_MAX + 1L,      /* { dg-warning "signed conversion from .long int. to .unsigned int. changes value from .4294967296. to .0." "lp64" { target lp64 } } */
  .ui = UINT_MAX + 1LU,     /* { dg-warning "conversion from .long unsigned int. to .unsigned int. changes value from .4294967296. to .0." "lp64" { target lp64 } } */

  .sl = LONG_MAX + 1LU,     /* { dg-warning "signed conversion from .long unsigned int. to .long int. changes value from .9223372036854775808. to .-9223372036854775808." "lp64" { target lp64 } } */
  /* { dg-warning "signed conversion from .long unsigned int. to .long int. changes value from .2147483648. to .-2147483648." "ilp32" { target ilp32 } .-1 } */
  .ul = ULONG_MAX + 1LU     /* there should be some warning here */
};
