/* CTF generation for functions with varargs or otherwise.

   In this testcase, it is expected to see one CTF_K_FUNCTION record with two
   function arguments.  The second function argument with a value of 0
   corresponds to the ellipsis.

   Example CTF section excerpt on x86_64 :

    .long   0x5     # ctt_name		    (name = format)
    .long   0x16000002      # ctt_info	    (CTF_K_FUNCTION with 2 arguments)
    .long   0x2     # ctt_size or ctt_type  (return typeID)
    .long   0x2     # dtu_argv		    (TypeID of the First argument)
    .long   0       # dtu_argv		    (TypeID of the second argument)
    .ascii "\0"     # ctf_string
    .ascii "int\0"  # ctf_string
    .ascii "format\0"       # ctf_string

    */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x16000002\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "dtu_argv" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*dtu_argv" 1 } } */

int foo (void);

int bar (int);

int * format (int * fmt, ...)
{
  return fmt;
}
