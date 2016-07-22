/* { dg-do compile }  */
/* { dg-additional-options "-std=gnu99" }  */

struct thread_info {
 struct task_struct *task;
};

static inline __attribute__((always_inline))
              __attribute__((no_instrument_function))
struct thread_info *current_thread_info(void)
{
 struct thread_info *ti;

 unsigned long __dummy;

 __asm__ __volatile__ (
  "mov	r15, %0\n\t"
  "and	%1, %0\n\t"
  : "=&r" (ti), "=r" (__dummy)
  : "1" (~((1 << 13) - 1))
  : "memory");

 return ti;
}

typedef struct seqcount {
 unsigned sequence;
} seqcount_t;

struct inode;

struct dentry {
 seqcount_t d_seq;
 struct inode *d_inode;
};

struct path {
 struct vfsmount *mnt;
 struct dentry *dentry;
};

struct file {
 struct path f_path;
} __attribute__((aligned(4)));

struct task_struct
{
 int link_count, total_link_count;
 struct fs_struct *fs;
};

struct fd {
 struct file *file;
 unsigned int flags;
};

static inline __attribute__((always_inline))
              __attribute__((no_instrument_function))
struct fd
fdget_raw(unsigned int fd)
{
  return (struct fd){(struct file *)(fd & ~3),fd & 3};
}


struct fs_struct;

struct nameidata {
 struct path path;
 struct path root;
 struct inode *inode;
 unsigned int flags;
 unsigned seq, m_seq;
 struct file *base;
};

int read_seqcount_retry(const seqcount_t *s, unsigned start);

int
path_init(int dfd, const char *name, unsigned int flags,
          struct nameidata *nd)
{
 int retval = 0;

 if (*name=='/') {
  nd->path = nd->root;
 } else if (dfd == -100) {

  if (flags & 0x0040) {
   struct fs_struct *fs = (current_thread_info()->task)->fs;
  }
 } else {

  struct fd f = fdget_raw(dfd);
  struct dentry *dentry;

  if (!f.file)
   return -9;

  dentry = f.file->f_path.dentry;

  nd->path = f.file->f_path;
  if (flags & 0x0040) {
   if (f.flags & 1)
    nd->base = f.file;
  }
 }

 nd->inode = nd->path.dentry->d_inode;
 if (!(flags & 0x0040))
  goto done;
 if (__builtin_expect(!!(!read_seqcount_retry(&nd->path.dentry->d_seq, nd->seq)), 1))
  goto done;
 if (!(nd->flags & 0x2000))
  nd->root.mnt = ((void *)0);

 return -10;
done:
 (current_thread_info()->task)->total_link_count = 0;
 return 0;
}
