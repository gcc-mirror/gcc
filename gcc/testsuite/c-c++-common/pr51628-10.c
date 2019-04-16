/* PR c/51628.  */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

struct pair_t
{
  char c;
  __int128_t i;
} __attribute__ ((packed));

typedef struct unaligned_int128_t_
{
  __int128_t value;
} __attribute__((packed, may_alias)) unaligned_int128_t;

struct pair_t p = {0, 1};
unaligned_int128_t *addr = (unaligned_int128_t *) &p.i;

int 
main() 
{
  addr->value = ~(__int128_t)0;
  return (p.i != 1) ? 0 : 1;
}
