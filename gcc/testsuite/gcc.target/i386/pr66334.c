/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -fpic -fexceptions -fasynchronous-unwind-tables" } */
/* { dg-final { scan-assembler "movl\[ \\t\].+, %ebx" } } */
extern int foo (int);
extern void exit (int __status) __attribute__ ((__nothrow__ )) __attribute__ ((__noreturn__));
struct __pthread_cleanup_frame
{
  void (*__cancel_routine) (void *);
  void *__cancel_arg;
  int __do_it;
  int __cancel_type;
};
extern __inline void
__pthread_cleanup_routine (struct __pthread_cleanup_frame *__frame)
{
  if (__frame->__do_it)
    __frame->__cancel_routine (__frame->__cancel_arg);
}
static int cl_called;

static void
cl (void *arg)
{
  ++cl_called;
}


void *
tf_usleep (void *arg)
{

  do { struct __pthread_cleanup_frame __clframe __attribute__ ((__cleanup__ (__pthread_cleanup_routine))) = { .__cancel_routine = (cl), .__cancel_arg = (
																			 ((void *)0)), .__do_it = 1 };;

    foo (arg == ((void *)0) ? (0x7fffffffL * 2UL + 1UL) : 0);

    __clframe.__do_it = (0); } while (0);

  exit (1);
}
