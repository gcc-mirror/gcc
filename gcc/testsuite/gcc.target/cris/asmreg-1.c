/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "\\\.ifnc \\\$r9-\\\$r10-\\\$r11-\\\$r12" } } */

/* Sanity check for asm register operands in syscall failed for
   cris-axis-linux-gnu due to regmove bug.
   Hans-Peter Nilsson <hp@axis.com>.  */

extern void lseek64 (int, long long, int);
extern int *__errno_location (void);
struct dirent64
{
  long long d_off;
  unsigned short int d_reclen;
  char d_name[256];
};
struct kernel_dirent64
{   
  long long d_off;
  unsigned short d_reclen;
  char d_name[256];
};

static inline int __attribute__ ((__always_inline__))
__syscall_getdents64 (int fd, char * dirp, unsigned count)
{
  register unsigned long __sys_res asm ("r10");
  register unsigned long __r10 __asm__ ("r10") = (unsigned long) fd;
  register unsigned long __r11 __asm__ ("r11") = (unsigned long) dirp;
  register unsigned long __r12 __asm__ ("r12") = (unsigned long) count;
  register unsigned long __callno asm ("r9") = (220);
  asm volatile (".ifnc %1-%0-%3-%4,$r9-$r10-$r11-$r12\n\t"
		".err\n\t"
		".endif\n\t"
		"break 13"
		: "=r" (__sys_res)
		: "r" (__callno), "0" (__r10), "r" (__r11), "r" (__r12)
		: "memory");
  if (__sys_res >= (unsigned long) -4096)
    {
      (*__errno_location ()) = - __sys_res;
      __sys_res = -1;
    }
  return __sys_res;
}

int
__getdents64 (int fd, char *buf, unsigned nbytes)
{   
  struct dirent64 *dp;
  long long last_offset = -1;
  int retval;
  struct kernel_dirent64 *skdp, *kdp;
  dp = (struct dirent64 *) buf;
  skdp = kdp = __builtin_alloca (nbytes);
  retval = __syscall_getdents64(fd, (char *)kdp, nbytes);
  if (retval == -1)
    return -1;
  while ((char *) kdp < (char *) skdp + retval)
    {
      if ((char *) dp > buf + nbytes)
	{
	  lseek64(fd, last_offset, 0);
	  break;
	}
      last_offset = kdp->d_off;
      __builtin_memcpy (dp->d_name, kdp->d_name, kdp->d_reclen - 10);
      dp = (struct dirent64 *) ((char *) dp + sizeof (*dp));
      kdp = (struct kernel_dirent64 *) (((char *) kdp) + kdp->d_reclen);
    }

  return (char *) dp - buf;
}
