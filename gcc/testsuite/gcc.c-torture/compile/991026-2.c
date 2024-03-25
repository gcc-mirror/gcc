typedef int __pid_t;			 
extern __pid_t fork  (void)    ;
extern int printf  (__const char *__restrict __format, ...)    ;
extern char *strerror  (int __errnum)    ;
extern int errno;
extern int *__errno_location  (void)     __attribute__ ((__const__));
extern void _exit  (int __status)   __attribute__ ((__noreturn__));
extern void exit  (int __status)     __attribute__ ((__noreturn__));
extern int close  (int __fd)    ;
extern int dup  (int __fd)    ;
extern int open  (__const char *__file, int __oflag, ...)    ;


char	myname[]="foo";

void
detach(void)
{
	switch(fork()) {
	    case -1:
		printf("%s: Error: fork - %s\n",myname, strerror((*__errno_location ()) ));
		exit(255);
	    case 0:
		 
		close(0);
		close(1);
		close(2);
		dup(dup(open("/dev/null", 02 )));
		return;
	    default:
		 
		_exit(0);
	}
}
