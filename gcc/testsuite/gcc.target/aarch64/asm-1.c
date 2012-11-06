
/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct
{
  int i;
  int y;
} __attribute__ ((aligned (16))) struct64_t;

void foo ()
{
  struct64_t tmp;
  asm volatile ("ldr q0, %[value]" : : [value]"m"(tmp));
}
