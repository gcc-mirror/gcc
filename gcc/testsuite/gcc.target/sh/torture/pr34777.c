/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-additional-options "-fschedule-insns -fPIC -mprefergot" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */

static __inline __attribute__ ((__always_inline__)) void *
_dl_mmap (void * start, int length, int prot, int flags, int fd,
	  int offset)
{
  register long __sc3 __asm__ ("r3") = 90;
  register long __sc4 __asm__ ("r4") = (long) start;
  register long __sc5 __asm__ ("r5") = (long) length;
  register long __sc6 __asm__ ("r6") = (long) prot;
  register long __sc7 __asm__ ("r7") = (long) flags;
  register long __sc0 __asm__ ("r0") = (long) fd;
  register long __sc1 __asm__ ("r1") = (long) offset;
  __asm__ __volatile__ ("trapa	%1"
			: "=z" (__sc0)
			: "i" (0x10 + 6), "0" (__sc0), "r" (__sc4),
			  "r" (__sc5), "r" (__sc6), "r" (__sc7),
			  "r" (__sc3), "r" (__sc1)
			: "memory" );
}

extern int _dl_pagesize;
void
_dl_dprintf(int fd, const char *fmt, ...)
{
  static char *buf;
  buf = _dl_mmap ((void *) 0, _dl_pagesize, 0x1 | 0x2, 0x02 | 0x20, -1, 0);
}
