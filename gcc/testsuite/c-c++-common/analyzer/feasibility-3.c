/* Reduced and adapted from Linux: fs/proc/inode.c: proc_reg_open
   (GPL v2.0).  */

/* { dg-additional-options "-Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

/* Types.  */

typedef unsigned char u8;
#ifndef __cplusplus
typedef _Bool bool;
#endif
typedef unsigned int gfp_t;

struct file;
struct kmem_cache;
struct proc_dir_entry;

struct inode { /* [...snip...] */ };

enum {
 PROC_ENTRY_PERMANENT = 1U << 0,
};

struct proc_ops {
 /* [...snip...] */
 int (*proc_open)(struct inode *, struct file *);
 /* [...snip...] */
 int (*proc_release)(struct inode *, struct file *);
 /* [...snip...] */
};

struct proc_dir_entry {
 /* [...snip...] */
 struct completion *pde_unload_completion;
 /* [...snip...] */
 union {
  const struct proc_ops *proc_ops;
  const struct file_operations *proc_dir_ops;
 };
 /* [...snip...] */
 u8 flags;
 /* [...snip...] */
};

struct pde_opener {
 /* [...snip...] */
 struct file *file;
 /* [...snip...] */
};

struct proc_inode {
 /* [...snip...] */
 struct proc_dir_entry *pde;
 /* [...snip...] */
 struct inode vfs_inode;
};

/* Data.  */

static struct kmem_cache *pde_opener_cache __attribute__((__section__(".data..ro_after_init")));

/* Functions. */

void *kmem_cache_alloc(struct kmem_cache *, gfp_t flags) __attribute__((__malloc__));
void kmem_cache_free(struct kmem_cache *, void *);

static inline bool pde_is_permanent(const struct proc_dir_entry *pde)
{
 return pde->flags & PROC_ENTRY_PERMANENT;
}

static inline struct proc_inode *PROC_I(const struct inode *inode)
{
  char *__mptr = (char *)(inode);
  return ((struct proc_inode *)(__mptr - __builtin_offsetof(struct proc_inode, vfs_inode)));
}

static inline struct proc_dir_entry *PDE(const struct inode *inode)
{
 return PROC_I(inode)->pde;
}

/* We don't want to emit bogus use of uninitialized value 'pdeo'
   warnings from -Wanalyzer-use-of-uninitialized-value in this function;
   these would require following infeasible paths in which "release" is
   first NULL (to avoid the initialization of "pdeo") and then is non-NULL
   (to access "pdeo").

   "release" is sufficiently complicated in this function to hit the
   complexity limit for symbolic values during enode exploration.  */

static int proc_reg_open(struct inode *inode, struct file *file)
{
 struct proc_dir_entry *pde = PDE(inode);
 int rv = 0;


 int (*open)(struct inode *, struct file *);
 int (*release)(struct inode *, struct file *);

 struct pde_opener *pdeo;

 if (pde_is_permanent(pde)) {
  open = pde->proc_ops->proc_open;
  if (open)
   rv = open(inode, file);
  return rv;
 }

 /* [...snip...] */

 release = pde->proc_ops->proc_release;
 if (release) {
  pdeo = (struct pde_opener *) kmem_cache_alloc(pde_opener_cache,
			  ((( gfp_t)(0x400u|0x800u))
			   | (( gfp_t)0x40u)
			   | (( gfp_t)0x80u)));
  if (!pdeo) {
   rv = -12;
   goto out_unuse;
  }
 }

 open = pde->proc_ops->proc_open;
 if (open)
  rv = open(inode, file);

 if (release) {
  if (rv == 0) {

   pdeo->file = file; /* { dg-bogus "uninit" } */
   /* [...snip...] */
  } else
   kmem_cache_free(pde_opener_cache, pdeo); /* { dg-bogus "uninit" } */
 }

out_unuse:
 /* [...snip...] */
 return rv;
}
