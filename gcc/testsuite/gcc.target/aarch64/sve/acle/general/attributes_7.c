/* { dg-options "-msve-vector-bits=256 -W -Wall" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))
#define GNU_ATTR __attribute__ ((vector_size (N / 8)))

typedef svint8_t fixed_int8_t FIXED_ATTR;
typedef svint16_t fixed_int16_t FIXED_ATTR;

typedef svuint8_t fixed_uint8_t FIXED_ATTR;

typedef svbool_t fixed_bool_t FIXED_ATTR;

typedef int8_t gnu_int8_t GNU_ATTR;
typedef int16_t gnu_int16_t GNU_ATTR;

typedef uint8_t gnu_uint8_t GNU_ATTR;

typedef int bad_type_1 __attribute__ ((arm_sve_vector_bits (N))); // { dg-error {'arm_sve_vector_bits' applied to non-SVE type 'int'} }
typedef svbool_t bad_type_2 __attribute__ ((arm_sve_vector_bits)); // { dg-error {wrong number of arguments specified for 'arm_sve_vector_bits' attribute} }
typedef svbool_t bad_type_3 __attribute__ ((arm_sve_vector_bits (N, N))); // { dg-error {wrong number of arguments specified for 'arm_sve_vector_bits' attribute} }
typedef svbool_t bad_type_4 __attribute__ ((arm_sve_vector_bits ("256"))); // { dg-error {'arm_sve_vector_bits' requires an integer constant expression} }
typedef svbool_t bad_type_5 __attribute__ ((arm_sve_vector_bits (100))); // { dg-warning {unsupported SVE vector size} }
typedef svint32x2_t bad_type_6 __attribute__ ((arm_sve_vector_bits (N))); // { dg-error {'arm_sve_vector_bits' applied to non-vector type 'svint32x2_t'} }
typedef svint8_t bad_type_7 __attribute__ ((arm_sve_vector_bits (N))) __attribute__ ((arm_sve_vector_bits (N))); // { dg-error {'arm_sve_vector_bits' applied to type 'svint8_t __attribute__\(\(arm_sve_vector_bits\([0-9]+\)\)\)', which already has a size} }
typedef fixed_bool_t bad_type_8 __attribute__ ((arm_sve_vector_bits (N))) __attribute__ ((arm_sve_vector_bits (N))); // { dg-error {'arm_sve_vector_bits' applied to type 'fixed_bool_t' {aka 'svbool_t __attribute__\(\(arm_sve_vector_bits\([0-9]+\)\)\)'}, which already has a size} }
typedef gnu_int8_t bad_type_9 __attribute__ ((arm_sve_vector_bits (N))) __attribute__ ((arm_sve_vector_bits (N))); // { dg-error {'arm_sve_vector_bits' applied to non-SVE type 'gnu_int8_t'} }
typedef svcount_t bad_type_10 __attribute__ ((arm_sve_vector_bits (N))); // { dg-error {'arm_sve_vector_bits' applied to non-vector type 'svcount_t'} }

void
f (int c)
{
  svint8_t ss8;
  fixed_int8_t fs8;
  gnu_int8_t gs8;

  svuint8_t su8;
  fixed_uint8_t fu8;
  gnu_uint8_t gu8;

  svint16_t ss16;
  fixed_int16_t fs16;
  gnu_int16_t gs16;

  svbool_t sb;
  fixed_bool_t fb;

  ss8 = ss8 + ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  ss8 = ss8 + fs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  ss8 = ss8 + gs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  ss8 += ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  ss8 += fs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  ss8 += gs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }

  fs8 = fs8 + ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  fs8 = fs8 + fs8;
  fs8 = fs8 + gs8; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  fs8 += ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  fs8 += fs8;
  fs8 += gs8; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }

  gs8 = gs8 + ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  gs8 = gs8 + fs8; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gs8 = gs8 + gs8;
  gs8 += ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\+} }
  gs8 += fs8; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gs8 += gs8;

  fs8 = ss8;
  fs8 = fs8;
  fs8 = gs8;

  fs8 = su8; // { dg-error {cannot convert|incompatible types} }
  fs8 = fu8; // { dg-error {cannot convert|incompatible types} }
  fs8 = gu8; // { dg-error {cannot convert|incompatible types} }

  fs8 = ss16; // { dg-error {cannot convert|incompatible types} }
  fs8 = fs16; // { dg-error {cannot convert|incompatible types} }
  fs8 = gs16; // { dg-error {cannot convert|incompatible types} }

  (void) (c ? ss8 : ss8);
  (void) (c ? ss8 : fs8); // { dg-error {type mismatch|different types} }
  (void) (c ? ss8 : gs8); // { dg-error {type mismatch|different types} }

  (void) (c ? fs8 : ss8); // { dg-error {type mismatch|different types} }
  (void) (c ? fs8 : fs8);
  (void) (c ? fs8 : gs8); // { dg-error {type mismatch|different types} }

  (void) (c ? gs8 : ss8); // { dg-error {type mismatch|different types} }
  (void) (c ? gs8 : fs8); // { dg-error {type mismatch|different types} }
  (void) (c ? gs8 : gs8);

  sb = fb;
  fb = sb;

  (void) (c ? sb : sb);
  (void) (c ? sb : fb); // { dg-error {type mismatch|different types} "$tool" { xfail c } }

  (void) (c ? fb : sb); // { dg-error {type mismatch|different types} "$tool" { xfail c } }
  (void) (c ? fb : fb);
}

void
g (int c)
{
  svint8_t *ss8;
  fixed_int8_t *fs8;
  gnu_int8_t *gs8;

  svuint8_t *su8;
  fixed_uint8_t *fu8;
  gnu_uint8_t *gu8;

  svint16_t *ss16;
  fixed_int16_t *fs16;
  gnu_int16_t *gs16;

  svbool_t *sb;
  fixed_bool_t *fb;

  __PTRDIFF_TYPE__ diff __attribute__((unused));
  void *select __attribute__((unused));

  diff = ss8 - ss8; // { dg-error {arithmetic on pointer to SVE type 'svint8_t'} }
  diff = ss8 - fs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\-} "" }
  diff = ss8 - gs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\-} "" }

  diff = fs8 - ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\-} "" }
  diff = fs8 - fs8;
  diff = fs8 - gs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\-} "" }

  diff = gs8 - ss8; // { dg-error {invalid operands [^\n]* binary[^\n]*\-} "" }
  diff = gs8 - fs8; // { dg-error {invalid operands [^\n]* binary[^\n]*\-} "" }
  diff = gs8 - gs8;

  fs8 = ss8; // { dg-error {invalid conversion} "" { target c++ } }
	     // { dg-error {incompatible pointer type} "c" { target c } .-1 }
  fs8 = fs8;
  fs8 = gs8; // { dg-error {invalid conversion} "" { target c++ } }
	     // { dg-error {incompatible pointer type} "c" { target c } .-1 }

  fs8 = su8; // { dg-error {cannot convert} "c++" { target c++ } }
	     // { dg-error {incompatible pointer type} "c" { target c } .-1 }
  fs8 = fu8; // { dg-error {cannot convert} "c++" { target c++ } }
	     // { dg-error {incompatible pointer type} "c" { target c } .-1 }
  fs8 = gu8; // { dg-error {cannot convert} "c++" { target c++ } }
	     // { dg-error {incompatible pointer type} "c" { target c } .-1 }

  fs8 = ss16; // { dg-error {cannot convert} "c++" { target c++ } }
	      // { dg-error {incompatible pointer type} "c" { target c } .-1 }
  fs8 = fs16; // { dg-error {cannot convert} "c++" { target c++ } }
	      // { dg-error {incompatible pointer type} "c" { target c } .-1 }
  fs8 = gs16; // { dg-error {cannot convert} "c++" { target c++ } }
	      // { dg-error {incompatible pointer type} "c" { target c } .-1 }

  select = c ? ss8 : ss8;
  select = c ? ss8 : fs8; // { dg-error {distinct pointer types} "" { target c++ } }
			  // { dg-error {pointer type mismatch} "c" { target c } .-1 }
  select = c ? ss8 : gs8; // { dg-error {distinct pointer types} "" { target c++ } }
			  // { dg-error {pointer type mismatch} "c" { target c } .-1 }

  select = c ? fs8 : ss8; // { dg-error {distinct pointer types} "" { target c++ } }
			  // { dg-error {pointer type mismatch} "c" { target c } .-1 }
  select = c ? fs8 : fs8;
  select = c ? fs8 : gs8; // { dg-error {distinct pointer types} "" { target c++ } }
			  // { dg-error {pointer type mismatch} "c" { target c } .-1 }

  select = c ? gs8 : ss8; // { dg-error {distinct pointer types} "" { target c++ } }
			  // { dg-error {pointer type mismatch} "c" { target c } .-1 }
  select = c ? gs8 : fs8; // { dg-error {distinct pointer types} "" { target c++ } }
			  // { dg-error {pointer type mismatch} "c" { target c } .-1 }
  select = c ? gs8 : gs8;

  diff = sb - sb; // { dg-error {arithmetic on pointer to SVE type 'svbool_t'} }
  diff = sb - fb; // { dg-error {invalid operands} "" { target c++ }  }
		  // { dg-error {arithmetic on pointer to SVE type 'svbool_t'} "c" { target c } .-1 }

  diff = fb - sb; // { dg-error {invalid operands} "" { target c++ }  }
		  // { dg-error {arithmetic on pointer to SVE type 'svbool_t'} "c" { target c } .-1 }
  diff = fb - fb;

  sb = fb; // { dg-error {invalid conversion} "" { target c++ } }
	   // { dg-warning {incompatible pointer type} "c" { target c xfail c } .-1 }
  fb = sb; // { dg-error {invalid conversion} "" { target c++ } }
	   // { dg-warning {incompatible pointer type} "c" { target c xfail c } .-1 }

  select = c ? sb : sb;
  select = c ? sb : fb; // { dg-error {distinct pointer types} "" { target c++ } }
			// { dg-warning {pointer type mismatch} "c" { target c xfail c } .-1 }

  select = c ? fb : sb; // { dg-error {distinct pointer types} "" { target c++ } }
			// { dg-warning {pointer type mismatch} "c" { target c xfail c } .-1 }
  select = c ? fb : fb;
}
