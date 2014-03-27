/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -mabi=apcs-gnu -march=armv7-a" } */

struct super_block
{
  int s_blocksize_bits;
};
struct btrfs_fs_info
{
  struct super_block *sb;
};
struct btrfs_root
{
  struct btrfs_fs_info *fs_info;
} *b;


int a, c, d;
long long e;

truncate_one_csum (struct btrfs_root *p1, long long p2, long long p3)
{
  int f, g, i = p1->fs_info->sb->s_blocksize_bits;
  g = a;
  long long h = p2 + p3;
  f = foo1 (b, 0, c, 0);
  e = f / g;
  e <<= p1->fs_info->sb->s_blocksize_bits;
  if (d < p2)
    {
      int j = e - h >> i;
      foo2 (p1, 0, j);
    }
  else
    {
      asm ("1\t.long ");
      __builtin_unreachable ();
    }
}
