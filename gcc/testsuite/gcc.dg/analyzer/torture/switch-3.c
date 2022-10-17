typedef unsigned int __u32;
__extension__ typedef unsigned long long __u64;

extern unsigned long
copy_from_user(void *to, const void *from, unsigned long n);

extern unsigned long
copy_to_user(void *to, const void *from, unsigned long n);

struct mtrr_sentry {
  __u64 base;
  __u32 size;
  __u32 type;
};

struct mtrr_gentry {
  __u64 base;
  __u32 size;
  __u32 regnum;
  __u32 type;
  __u32 _pad;
};

#define _IOC_NRBITS	8
#define _IOC_TYPEBITS	8
#define _IOC_SIZEBITS	14
#define _IOC_DIRBITS	2

#define _IOC_NRSHIFT	0
#define _IOC_TYPESHIFT	(_IOC_NRSHIFT+_IOC_NRBITS)
#define _IOC_SIZESHIFT	(_IOC_TYPESHIFT+_IOC_TYPEBITS)
#define _IOC_DIRSHIFT	(_IOC_SIZESHIFT+_IOC_SIZEBITS)

#define _IOC_WRITE	1U
#define _IOC_READ	2U

#define _IOC(dir,type,nr,size) \
	(((dir)  << _IOC_DIRSHIFT) | \
	 ((type) << _IOC_TYPESHIFT) | \
	 ((nr)   << _IOC_NRSHIFT) | \
	 ((size) << _IOC_SIZESHIFT))

#define _IOC_TYPECHECK(t) (sizeof(t))

#define _IOW(type,nr,size)	_IOC(_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
#define _IOWR(type,nr,size)	_IOC(_IOC_READ|_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))

#define MTRR_IOCTL_BASE 'M'

#define	EFAULT		14
#define	EINVAL		22
#define	ENOTTY		25

#define MTRRIOC_ADD_ENTRY        _IOW(MTRR_IOCTL_BASE,  0, struct mtrr_sentry)
#define MTRRIOC_SET_ENTRY        _IOW(MTRR_IOCTL_BASE,  1, struct mtrr_sentry)
#define MTRRIOC_DEL_ENTRY        _IOW(MTRR_IOCTL_BASE,  2, struct mtrr_sentry)
#define MTRRIOC_GET_ENTRY        _IOWR(MTRR_IOCTL_BASE, 3, struct mtrr_gentry)
#define MTRRIOC_KILL_ENTRY       _IOW(MTRR_IOCTL_BASE,  4, struct mtrr_sentry)
#define MTRRIOC_ADD_PAGE_ENTRY   _IOW(MTRR_IOCTL_BASE,  5, struct mtrr_sentry)
#define MTRRIOC_SET_PAGE_ENTRY   _IOW(MTRR_IOCTL_BASE,  6, struct mtrr_sentry)
#define MTRRIOC_DEL_PAGE_ENTRY   _IOW(MTRR_IOCTL_BASE,  7, struct mtrr_sentry)
#define MTRRIOC_GET_PAGE_ENTRY   _IOWR(MTRR_IOCTL_BASE, 8, struct mtrr_gentry)
#define MTRRIOC_KILL_PAGE_ENTRY  _IOW(MTRR_IOCTL_BASE,  9, struct mtrr_sentry)

extern void check_init_u64 (__u64 v);
extern void check_init_u32 (__u32 v);

/* Adapted/reduced from arch/x86/kernel/cpu/mtrr/if.c: mtrr_ioctl,
   which is GPL-2.0  */

long mtrr_ioctl(unsigned int cmd, unsigned long __arg) {
  int err = 0;
  struct mtrr_sentry sentry;
  struct mtrr_gentry gentry;
  void *arg = (void *)__arg;

  __builtin_memset(&gentry, 0, sizeof(gentry));

  switch (cmd) {
	case MTRRIOC_ADD_ENTRY:
	case MTRRIOC_SET_ENTRY:
	case MTRRIOC_DEL_ENTRY:
	case MTRRIOC_KILL_ENTRY:
	case MTRRIOC_ADD_PAGE_ENTRY:
	case MTRRIOC_SET_PAGE_ENTRY:
	case MTRRIOC_DEL_PAGE_ENTRY:
	case MTRRIOC_KILL_PAGE_ENTRY:
		if (copy_from_user(&sentry, arg, sizeof(sentry)))
			return -EFAULT;
		break;
	case MTRRIOC_GET_ENTRY:
	case MTRRIOC_GET_PAGE_ENTRY:
		if (copy_from_user(&gentry, arg, sizeof(gentry)))
			return -EFAULT;
		break;
  }

  switch (cmd) {
	default:
		return -ENOTTY;
	case MTRRIOC_ADD_ENTRY:
	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_SET_ENTRY:
	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_DEL_ENTRY:
	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_KILL_ENTRY:
 	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_GET_ENTRY:
 	  check_init_u64 (gentry.base);
	  check_init_u32 (gentry.size);
	  check_init_u32 (gentry.regnum);
	  check_init_u32 (gentry.type);
	  check_init_u32 (gentry._pad);
	  break;
	case MTRRIOC_ADD_PAGE_ENTRY:
 	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_SET_PAGE_ENTRY:
 	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_DEL_PAGE_ENTRY:
 	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_KILL_PAGE_ENTRY:
 	  check_init_u64 (sentry.base);
	  check_init_u32 (sentry.size);
	  check_init_u32 (sentry.type);
	  break;
	case MTRRIOC_GET_PAGE_ENTRY:
 	  check_init_u64 (gentry.base);
	  check_init_u32 (gentry.size);
	  check_init_u32 (gentry.regnum);
	  check_init_u32 (gentry.type);
	  check_init_u32 (gentry._pad);
	  break;
  }

 return err;
}
