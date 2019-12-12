/* PR c/51628.  */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O" } */

struct tuple_t
{
  char c[10];
  __int128_t i;
} __attribute__((packed, aligned (8)));

typedef struct unaligned_int128_t_
{
  __int128_t value;
} __attribute__ ((packed, aligned(4))) unaligned_int128_t;

struct tuple_t p = {{0}, 1};
unaligned_int128_t *addr = (unaligned_int128_t *)(&p.i);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
