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

