/* Asm operands that are given as hard registers must keep the same
   hard register all the way through compilation.  Example derived
   from glibc source.  */
/* { dg-do compile } */
/* { dg-options "-O2 -frename-registers -fcprop-registers" } */
/* { dg-final { scan-assembler "callsys1 .0 .19 .0 .16 .17" } } */
/* { dg-final { scan-assembler "callsys2 .0 .19 .0 .16 .17" } } */

struct stat {
  int dummy;
};

struct kernel_stat {
  int dummy;
};

extern int xstat_conv (int vers, struct kernel_stat *kbuf, void *ubuf);
extern int *__errno_location (void) __attribute__ ((__const__));

int
__fxstat (int vers, int fd, struct stat *buf)
{
  struct kernel_stat kbuf;
  int result;

  if (vers == 0)
    return
      ({
	long _sc_ret, _sc_err;
	{
	  register long _sc_0 __asm__("$0");
	  register long _sc_16 __asm__("$16");
	  register long _sc_17 __asm__("$17");
	  register long _sc_19 __asm__("$19");
	  _sc_0 = 91;
	  _sc_16 = (long) (fd);
	  _sc_17 = (long) (((struct kernel_stat *) buf));
	  __asm__("callsys1 %0 %1 %2 %3 %4"
		  : "=r"(_sc_0), "=r"(_sc_19)
		  : "0"(_sc_0), "r"(_sc_16), "r"(_sc_17)
		  : "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8",
		    "$22", "$23", "$24", "$25", "$27", "$28", "memory");
	  _sc_ret = _sc_0, _sc_err = _sc_19;
	}
	if (_sc_err)
	  {
	    (*__errno_location ()) = (_sc_ret);
	    _sc_ret = -1L;
	  }
	_sc_ret;
      });

  result =
      ({
	long _sc_ret, _sc_err;
	{
	  register long _sc_0 __asm__("$0");
	  register long _sc_16 __asm__("$16");
	  register long _sc_17 __asm__("$17");
	  register long _sc_19 __asm__("$19");
	  _sc_0 = 91;
	  _sc_16 = (long) (fd);
	  _sc_17 = (long) ((&kbuf));
	  __asm__("callsys2 %0 %1 %2 %3 %4"
		  : "=r"(_sc_0), "=r"(_sc_19)
		  : "0"(_sc_0), "r"(_sc_16), "r"(_sc_17)
		  : "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8",
		    "$22", "$23", "$24", "$25", "$27", "$28", "memory");
	  _sc_ret = _sc_0, _sc_err = _sc_19;
	}
	if (_sc_err)
	  {
	    (*__errno_location ()) = (_sc_ret);
	    _sc_ret = -1L;
	  }
	_sc_ret;
      });
  if (result == 0)
    result = xstat_conv (vers, &kbuf, buf);

  return result;
}
