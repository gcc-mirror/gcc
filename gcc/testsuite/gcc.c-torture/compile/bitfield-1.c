/* { dg-require-effective-target int128 } */

struct f
{
  __uint128_t t:124;
  __uint128_t t1:4;
};

struct f g(void)
{
  struct f t = {1, 2};
  return t;
}
