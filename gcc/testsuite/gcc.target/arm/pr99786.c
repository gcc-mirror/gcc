/* { dg-do compile } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mcpu=*" } { "-mcpu=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mabi=*" } { "-mabi=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-march=*" } { "-march=iwmmxt" } } */
/* { dg-skip-if "Test is specific to ARM mode" { arm*-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_iwmmxt_ok } */
/* { dg-options "-O3 -mcpu=iwmmxt" } */

typedef signed char V __attribute__((vector_size (8))); 

void
foo (V *a) 
{ 
  *a = *a * 3; 
}

typedef signed short Vshort __attribute__((vector_size (8))); 
void
foo_short (Vshort *a) 
{ 
  *a = *a * 3; 
}

typedef signed int Vint __attribute__((vector_size (8))); 
void
foo_int (Vint *a) 
{ 
  *a = *a * 3; 
}
