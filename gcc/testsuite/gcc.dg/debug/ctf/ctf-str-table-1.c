/* CTF String Table as generated by the compiler is expected to have only a
   single empty string.  Just an optimization by the compiler, it is not
   mandated by the CTF format.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "ascii \".0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

union wait
{
  int w_status;
  struct
    {
      int __w_termsig;
      int __w_coredump;
    } __wait_terminated;
   struct
    {
      int __w_stopval;
      int __w_stopsig;
    } __wait_stopped;
};

typedef union { union wait * __uptr; int * iptr; } __WAIT_STATUS;

__WAIT_STATUS waitstatus;
