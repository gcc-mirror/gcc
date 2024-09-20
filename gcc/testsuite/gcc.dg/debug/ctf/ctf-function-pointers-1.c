/* CTF generation of function pointers.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "\[\t \]0x16000001\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x16000002\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x16000003\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"__foo_fn.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"destroy.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"func.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

int (*func) (int *, char);

typedef int (*__foo_fn) (void *__cookie, char *__buf, int __nbytes);

typedef struct object
{
   int myint;
   char mychar;
   void (*destroy)(struct object *);
} object_t;

object_t myobj;
__foo_fn fooit;
