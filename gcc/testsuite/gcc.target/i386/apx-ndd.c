/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-not "movl"} } */

#include <stdint.h>

#define FOO(TYPE, OP_NAME, OP)   \
TYPE				 \
__attribute__ ((noipa)) 	 \
foo_##OP_NAME##_##TYPE (TYPE *a) \
{				 \
  TYPE b = *a OP 1;		 \
  return b;			 \
}			

#define FOO1(TYPE, OP_NAME, OP)		 \
TYPE				  	 \
__attribute__ ((noipa)) 	  	 \
foo1_##OP_NAME##_##TYPE (TYPE a, TYPE b) \
{				 	 \
  TYPE c = a OP b;		 	 \
  return c;			 	 \
}			

#define FOO2(TYPE, OP_NAME, OP)		  \
TYPE				  	  \
__attribute__ ((noipa)) 	  	  \
foo2_##OP_NAME##_##TYPE (TYPE *a, TYPE b) \
{				 	  \
  TYPE c = *a OP b;		 	  \
  return c;			 	  \
}

#define FOO3(TYPE, OP_NAME, OP, IMM)  \
TYPE				      \
__attribute__ ((noipa))		      \
foo3_##OP_NAME##_##TYPE (TYPE a)      \
{				      \
  TYPE b = a OP IMM;		      \
  return b;			      \
}			

#define FOO4(TYPE, OP_NAME, OP1, OP2, IMM1)		    \
TYPE							    \
__attribute__ ((noipa))					    \
foo4_##OP_NAME##_##TYPE (TYPE a)			    \
{							    \
  TYPE b = (a OP1 IMM1 | a OP2 (8 * sizeof(TYPE) - IMM1));  \
  return b;						    \
}

#define F(TYPE, OP_NAME, OP)   \
TYPE				 \
__attribute__ ((noipa)) 	 \
f_##OP_NAME##_##TYPE (TYPE *a) \
{				 \
  TYPE b = OP*a;		 \
  return b;			 \
}			

#define F1(TYPE, OP_NAME, OP)		 \
TYPE				  	 \
__attribute__ ((noipa)) 	  	 \
f1_##OP_NAME##_##TYPE (TYPE a) \
{				 	 \
  TYPE b = OP a;		 	 \
  return b;			 	 \
}			
FOO (char, add, +)
FOO1 (char, add, +)
FOO2 (char, add, +)
FOO (short, add, +)
FOO1 (short, add, +)
FOO2 (short, add, +)
FOO (int, add, +)
FOO1 (int, add, +)
FOO2 (int, add, +)
FOO (int64_t, add, +)
FOO1 (int64_t, add, +)
FOO2 (int64_t, add, +)

FOO (char, sub, -)
FOO1 (char, sub, -)
FOO (short, sub, -)
FOO1 (short, sub, -)
FOO (int, sub, -)
FOO1 (int, sub, -)
FOO (int64_t, sub, -)
FOO1 (int64_t, sub, -)

F (char, neg, -)
F1 (char, neg, -)
F (short, neg, -)
F1 (short, neg, -)
F (int, neg, -)
F1 (int, neg, -)
F (int64_t, neg, -)
F1 (int64_t, neg, -)

F (char, not, ~)
F1 (char, not, ~)
F (short, not, ~)
F1 (short, not, ~)
F (int, not, ~)
F1 (int, not, ~)
F (int64_t, not, ~)
F1 (int64_t, not, ~)

FOO (char, and, &)
FOO1 (char, and, &)
FOO (short, and, &)
FOO1 (short, and, &)
FOO (int, and, &)
FOO1 (int, and, &)
FOO (int64_t, and, &)
FOO1 (int64_t, and, &)

FOO (char, or, |)
FOO1 (char, or, |)
FOO (short, or, |)
FOO1 (short, or, |)
FOO (int, or, |)
FOO1 (int, or, |)
FOO (int64_t, or, |)
FOO1 (int64_t, or, |)

FOO (char, xor, ^)
FOO1 (char, xor, ^)
FOO (short, xor, ^)
FOO1 (short, xor, ^)
FOO (int, xor, ^)
FOO1 (int, xor, ^)
FOO (int64_t, xor, ^)
FOO1 (int64_t, xor, ^)

FOO (char, shl, <<)
FOO3 (char, shl, <<, 7)
FOO (short, shl, <<)
FOO3 (short, shl, <<, 7)
FOO (int, shl, <<)
FOO3 (int, shl, <<, 7)
FOO (int64_t, shl, <<)
FOO3 (int64_t, shl, <<, 7)

FOO (char, sar, >>)
FOO3 (char, sar, >>, 7)
FOO (short, sar, >>)
FOO3 (short, sar, >>, 7)
FOO (int, sar, >>)
FOO3 (int, sar, >>, 7)
FOO (int64_t, sar, >>)
FOO3 (int64_t, sar, >>, 7)

FOO (uint8_t, shr, >>)
FOO3 (uint8_t, shr, >>, 7)
FOO (uint16_t, shr, >>)
FOO3 (uint16_t, shr, >>, 7)
FOO (uint32_t, shr, >>)
FOO3 (uint32_t, shr, >>, 7)
FOO (uint64_t, shr, >>)
FOO3 (uint64_t, shr, >>, 7)

FOO4 (uint8_t, ror, >>, <<, 1)
FOO4 (uint16_t, ror, >>, <<, 1)
FOO4 (uint32_t, ror, >>, <<, 1)
FOO4 (uint64_t, ror, >>, <<, 1)

FOO4 (uint8_t, rol, <<, >>, 1)
FOO4 (uint16_t, rol, <<, >>, 1)
FOO4 (uint32_t, rol, <<, >>, 1)
FOO4 (uint64_t, rol, <<, >>, 1)

/* { dg-final { scan-assembler-times "add(?:b|l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "lea(?:l|q)\[^\n\r]\\(%r(?:d|s)i,%r(?:d|s)i\\), %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "add(?:b|l|w|q)\[^\n\r]%(?:|r|e)si(?:|l), \\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "sub(?:b|l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "sub(?:b|l|w|q)\[^\n\r]%(?:|r|e)si(?:|l), %(?:|r|e)di, %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "negb\[^\n\r]\\(%(?:r|e)di\\), %al" 1 } } */
/* { dg-final { scan-assembler-times "neg(?:l|w|q)\[^\n\r]\\(%(?:r|e)di\\), %(?:|r|e)ax" 3 } } */
/* { dg-final { scan-assembler-times "neg(?:l|w|q)\[^\n\r]%(?:|r|e)di, %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "not(?:b|l|w|q)\[^\n\r]\\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "not(?:l|w|q)\[^\n\r]%(?:|r|e)di, %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "andb\[^\n\r]*1, \\(%(?:r|e)di\\), %al" 1 } } */
/* { dg-final { scan-assembler-times "and(?:l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)ax" 3 } } */
/* { dg-final { scan-assembler-times "and(?:l|w|q)\[^\n\r]%(?:|r|e)di, %(?:|r|e)si, %(?:|r|e)ax" 2 } } */
/* { dg-final { scan-assembler-times "and(?:l|w|q)\[^\n\r]%(?:|r|e)si, %(?:|r|e)di, %(?:|r|e)ax" 2 } } */
/* { dg-final { scan-assembler-times "orb\[^\n\r]*1, \\(%(?:r|e)di\\), %al" 2} } */
/* { dg-final { scan-assembler-times "or(?:l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)ax" 6 } } */
/* { dg-final { scan-assembler-times "or(?:l|w|q)\[^\n\r]%(?:|r|e)di, %(?:|r|e)si, %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "or(?:l|w|q)\[^\n\r]%(?:|r|e)si, %(?:|r|e)di, %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "xorb\[^\n\r]*1, \\(%(?:r|e)di\\), %al" 1 } } */
/* { dg-final { scan-assembler-times "xor(?:l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)ax" 3 } } */
/* { dg-final { scan-assembler-times "xor(?:l|w|q)\[^\n\r]%(?:|r|e)di, %(?:|r|e)si, %(?:|r|e)ax" 2 } } */
/* { dg-final { scan-assembler-times "xor(?:l|w|q)\[^\n\r]%(?:|r|e)si, %(?:|r|e)di, %(?:|r|e)ax" 2 } } */
/* { dg-final { scan-assembler-times "sal(?:b|l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "sal(?:l|w|q)\[^\n\r]*7, %(?:|r|e)di, %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "sar(?:b|l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "sar(?:b|l|w|q)\[^\n\r]*7, %(?:|r|e)di(?:|l), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "shr(?:b|l|w|q)\[^\n\r]*1, \\(%(?:r|e)di\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "shr(?:b|l|w|q)\[^\n\r]*7, %(?:|r|e)di(?:|l), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "ror(?:b|l|w|q)\[^\n\r]*1, %(?:|r|e)di(?:|l), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "rol(?:b|l|w|q)\[^\n\r]*1, %(?:|r|e)di(?:|l), %(?:|r|e)a(?:x|l)" 4 } } */
