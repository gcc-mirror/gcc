/* { dg-do compile } */

struct timespec
{
  long tv_sec;
  long tv_nsec;
};
struct inode
{
  struct timespec i_atime;
  struct timespec i_mtime;
};
struct afs_vnode
{
  struct inode vfs_inode;
};
static inline
  struct inode *AFS_VNODE_TO_I (struct afs_vnode *vnode)
{
  return &vnode->vfs_inode;
};
void
afs_inode_map_status (struct afs_vnode *vnode)
{
  struct inode *inode = AFS_VNODE_TO_I (vnode);
  inode->i_atime = inode->i_mtime;
}
