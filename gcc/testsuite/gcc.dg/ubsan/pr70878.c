/* PR sanitizer/80878 */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-fsanitize=object-size" } */

void * sbrk ()
{
 volatile register unsigned int sp_r1 __asm__ ("ebx");
 return __builtin_strcat ((char*)sp_r1, 0); /* { dg-warning "cast to pointer from integer of different size" } */
}
