/* { dg-do compile { target ia32 } } */
/* { dg-options "-w" } */

typedef unsigned int size_t;	
__extension__ typedef long int __off_t;
typedef __off_t off_t;
static void *__sys_mmap(void *addr, size_t length, int prot, int flags, int fd,
			off_t offset)
{
  offset >>= 12;
  return (void *)({ long _ret;
      register long _num asm("eax") = (192);
      register long _arg1 asm("ebx") = (long)(addr);
      register long _arg2 asm("ecx") = (long)(length);
      register long _arg3 asm("edx") = (long)(prot);
      register long _arg4 asm("esi") = (long)(flags);
      register long _arg5 asm("edi") = (long)(fd);
      long _arg6 = (long)(offset);
      asm volatile ("pushl	%[_arg6]\n\t"
		    "pushl	%%ebp\n\t"
		    "movl	4(%%esp), %%ebp\n\t"
		    "int	$0x80\n\t"
		    "popl	%%ebp\n\t"
		    "addl	$4,%%esp\n\t"
		    : "=a"(_ret)
		    : "r"(_num), "r"(_arg1), "r"(_arg2), "r"(_arg3), "r"(_arg4),"r"(_arg5), [_arg6]"m"(_arg6)
		    : "memory", "cc" );
      _ret; });
}

int main(void)
{
  __sys_mmap(((void *)0), 0x1000, 0x1 | 0x2, 0x20 | 0x02, -1, 0);
  return 0;
}
