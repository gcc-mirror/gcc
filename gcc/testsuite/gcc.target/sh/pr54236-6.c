/* In this snippet, there was a missed subc case:
	tst	#1,r0
	movt	r0
	neg	r0,r0

   which should be:
	tst	#1,r0
	subc	r0,r0
*/

/* { dg-do compile }  */
/* { dg-options "-O2" }  */

/* { dg-final { scan-assembler-times {tst	#1,r0} 1 } }  */
/* { dg-final { scan-assembler-times {subc	r} 1 } }  */

/* { dg-final { scan-assembler-not "movt|not\t|neg\t|movrt" } }  */


struct inode
{
  unsigned int i_gid;
};

struct iattr
{
  unsigned int ia_valid;
  unsigned int ia_gid;
};

struct task_struct
{
  unsigned long flags;
  unsigned int cap_effective;
};

extern int in_group_p (unsigned int);

static inline struct task_struct*
get_current (void)
{
  struct task_struct *current;
  return current;
}

static inline int
capable (int cap)
{
  if (((get_current()->cap_effective) & (1 << (cap))))
    {
      get_current()->flags |= 0x00000100;
      return 1;
    }
  return 0;
}

int
inode_change_ok (struct inode *inode, struct iattr *attr)
{
  int retval = -1;
  unsigned int ia_valid = attr->ia_valid;

  if (ia_valid & 512)
    goto fine;

  if ((ia_valid & 4)
      && (!in_group_p(attr->ia_gid) && attr->ia_gid != inode->i_gid)
      && !capable(0))
    goto error;

fine:
 retval = 0;
error:
 return retval;
}
