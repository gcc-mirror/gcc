/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=arc700 -O2 -fpic" } */

/* Test if the compiler generates a constant address having that uses
   a neg keyword on the pic unspec.  */

typedef unsigned int uint32_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned long long int uint64_t;

enum type {
 t_undef = 0x01,
 t_group = 0x02,
 t_partition = 0x04,
 t_spare = 0x08,
 t_linear = 0x10,
 t_raid0 = 0x20,
 t_raid1 = 0x40,
 t_raid4 = 0x80,
 t_raid5_ls = 0x100,
 t_raid5_rs = 0x200,
 t_raid5_la = 0x400,
 t_raid5_ra = 0x800,
 t_raid6 = 0x1000,
};

struct raid_set {
  enum type type;
};

void
_find_factors (struct raid_set *rs, uint8_t * div, uint8_t * sub)
{
  struct factors {
    const uint8_t level;
    const uint8_t div, sub;
  };
  static struct factors factors[] = {
    {0, 1, 0},
    {1, 2, 0},
    {2, 2, 0},
    {5, 1, 1},
  };
  struct factors *f = (factors + (sizeof (factors) / sizeof (*factors)));

  while (f-- > factors) {
    if (rs->type == f->level) {
      *div = f->div;
      *sub = f->sub;
      return;
    }
  }

  *div = 1;
  *sub = 0;
}
