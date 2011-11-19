/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge" } */

struct inode
{
  unsigned short i_mode;
  unsigned int i_flags;
};

static inline int
is_sxid (unsigned int mode)
{
  return (mode & 0004000) || ((mode & 0002000) && (mode & 00010));
};

void
gfs2_set_inode_flags (int ip, struct inode *inode)
{
  unsigned int flags = inode->i_flags;
  if ((ip == 0) && !is_sxid (inode->i_mode))
    inode->i_flags |= 4096;
  inode->i_flags = flags;
}
