/* PR sanitizer/79904 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-psabi" } */

typedef signed char V __attribute__((vector_size (8))); 

void
foo (V *a) 
{ 
  *a = *a * 3; 
}
