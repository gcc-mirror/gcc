/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-not "movl"} } */

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

FOO (char, add, +)
FOO1 (char, add, +)
FOO2 (char, add, +)
FOO (short, add, +)
FOO1 (short, add, +)
FOO2 (short, add, +)
FOO (int, add, +)
FOO1 (int, add, +)
FOO2 (int, add, +)
FOO (long, add, +)
FOO1 (long, add, +)
FOO2 (long, add, +)

FOO (char, sub, -)
FOO1 (char, sub, -)
FOO (short, sub, -)
FOO1 (short, sub, -)
FOO (int, sub, -)
FOO1 (int, sub, -)
FOO (long, sub, -)
FOO1 (long, sub, -)
/* { dg-final { scan-assembler-times "add(?:b|l|w|q)\[^\n\r]*1, \\(%rdi\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "lea(?:l|q)\[^\n\r]\\(%r(?:d|s)i,%r(?:d|s)i\\), %(?:|r|e)ax" 4 } } */
/* { dg-final { scan-assembler-times "add(?:b|l|w|q)\[^\n\r]%(?:|r|e)si(?:|l), \\(%rdi\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "sub(?:b|l|w|q)\[^\n\r]*1, \\(%rdi\\), %(?:|r|e)a(?:x|l)" 4 } } */
/* { dg-final { scan-assembler-times "sub(?:b|l|w|q)\[^\n\r]%(?:|r|e)si(?:|l), %(?:|r|e)di, %(?:|r|e)a(?:x|l)" 4 } } */
