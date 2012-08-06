/* { dg-do run } */
/* { dg-options "-std=c99" } */

#include <stdint.h>

extern void abort (void);

#define N_DEV_BITS_4 5
#define N_INO_BITS_4 (32 - N_DEV_BITS_4 - 2 - 1)

#define N_DEV_BITS_8 8
#define N_INO_BITS_8 (64 - N_DEV_BITS_8 - 2 - 1)

struct dev_ino_4
{
  uint32_t mode:2;
  uint32_t short_ino:N_INO_BITS_4;
  uint32_t mapped_dev:N_DEV_BITS_4;
  uint32_t always_set:1;
};

struct dev_ino_8
{
  uint32_t mode:2;
  uint64_t short_ino:N_INO_BITS_8;
  uint32_t mapped_dev:N_DEV_BITS_8;
  uint32_t always_set:1;
};

struct dev_ino_full
{
  uint32_t mode:2;
  uint32_t dev;
  uint32_t ino;
};

enum di_mode
{
  DI_MODE_4 = 1,
  DI_MODE_8 = 2,
  DI_MODE_FULL = 3
};

struct di_ent
{
  union
  {
    struct dev_ino_4 di4;
    struct dev_ino_8 di8;
    struct dev_ino_full full;
    uint32_t u32;
    uint64_t u64;
    void *ptr;
  } u;
};

static struct di_ent
decode_ptr (struct di_ent const *v)
{
  struct di_ent di;
  di.u.ptr = (void *) v;
  return di;
}

static int
di_ent_equal (void const *x, void const *y)
{
  struct di_ent a = decode_ptr (x);
  struct di_ent b = decode_ptr (y);
  if (a.u.di4.mode != b.u.di4.mode)
    return 0;

  if (a.u.di4.mode == DI_MODE_4)
    return (a.u.di4.short_ino == b.u.di4.short_ino
            && a.u.di4.mapped_dev == b.u.di4.mapped_dev);

  if (a.u.di8.mode == DI_MODE_8)
    return (a.u.di8.short_ino == b.u.di8.short_ino
            && a.u.di8.mapped_dev == b.u.di8.mapped_dev);

  return (a.u.full.ino == b.u.full.ino
          && a.u.full.dev == b.u.full.dev);
}

int
main ()
{
  if (di_ent_equal ((void *) 0x80143c4d, (void *) 0x80173851) != 0)
    abort ();
  return 0;
}
