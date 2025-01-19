/* CTF generation of function pointers.

   In this testcase,  there is a single function type expected for two
   different function pointer types.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA -std=gnu17" } */
/* { dg-final { scan-assembler-times "\[\t \]0x16000001\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"rcu_callback_t.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"func.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

struct callback_head {
      struct callback_head *next;
        void (*func) (struct callback_head *head);
} __attribute__ (( aligned (sizeof (void *))));
#define rcu_head callback_head

struct callback_head chead;

typedef void (*rcu_callback_t) (struct rcu_head *head);

rcu_callback_t rcb;
