/* { dg-do compile } */
/* { dg-options "-mabi=altivec" { target { { powerpc*-*-linux* } && ilp32 } } } */
/* { dg-prune-output "operand types are" } */
/* Ignore warning on some powerpc-ibm-aix configurations. */
/* { dg-prune-output "non-standard ABI extension" } */

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

void
foo (vector (4, int) x, vector (4, float) y)
{
  vector (4, int) p4;
  vector (4, int) r4;
  vector (4, unsigned int) q4;
  vector (8, int) r8;
  vector (4, float) f4;
  
  r4 = x > y;	    /* { dg-error "comparing vectors with different element types" } */
  r8 = (x != p4);   /* { dg-error "incompatible types when assigning to type|cannot convert" } */
  r8 == r4;	    /* { dg-error "comparing vectors with different number of elements" } */
}
