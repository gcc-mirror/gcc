/* { dg-do compile } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-options "-O2 -mapcs -march=armv7-a -mfloat-abi=hard -mfpu=vfpv3-d16" } */

int inode_permission (), try_break_deleg ();
int mutex_lock (), mutex_unlock ();
struct mutex
{
};
struct dentry
{
  struct inode *d_inode;
};
struct inode
{
  const struct inode_operations *i_op;
  struct super_block *i_sb;
    union
    {
      const unsigned int i_nlink;
    };
  unsigned long i_state;
  struct mutex i_mutex;
};
struct super_block
{
  unsigned int s_max_links;
};
struct inode_operations
{
  int (*link) (struct dentry *, struct inode *, struct dentry *);
} __attribute__ ((__aligned__ ((1 << 6))));
static inline __attribute__ ((always_inline))
__attribute__ ((no_instrument_function))
int may_create (struct inode *dir, struct dentry *child)
{
  if (child->d_inode)
    return -17;
  return inode_permission (dir, 0x00000002 | 0x00000001);
}

int
vfs_link (struct dentry *old_dentry, struct inode *dir,
	  struct dentry *new_dentry, struct inode **delegated_inode)
{
  struct inode *inode = old_dentry->d_inode;
  unsigned max_links = dir->i_sb->s_max_links;
  int error;
  error = may_create (dir, new_dentry);
  if (error)
    return error;
  mutex_lock (&inode->i_mutex);
  if (inode->i_nlink == 0 && !(inode->i_state & (1 << 10)))
    error = -2;
  else if (max_links && inode->i_nlink >= max_links)
    error = -31;
  else
    {
      error = try_break_deleg (inode, delegated_inode);
      error = dir->i_op->link (old_dentry, dir, new_dentry);
    }
  mutex_unlock (&inode->i_mutex);
  return error;
}
