/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */
/* { dg-additional-options "-lpthread" } */

#include <pthread.h>

#ifdef __cplusplus
extern "C" {
#endif
extern int printf (char const *, ...);
#ifdef __cplusplus
}
#endif
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __UINT64_TYPE__ uint64_t;

/* Test that tags are checked across different threads.
   i.e. if this thread tries to access a different threads memory with the
   incorrect tag, then this thread fails.  */
void *
failing_thread_function (void *argument)
{
    void * other = (void *)((uint64_t)argument & 0xffffffffffffffULL);
    int *num = (int*)argument;
    printf ("(should succeed): first number = %d\n", num[0]);
    printf ("(now should fail):\n");

    int *othernum = (int*)other;
    printf (" second number = %d\n", othernum[0]);
    return (void *)1;
}

int
main (int argc, char **argv)
{
    int argument[100] = {0};
    argument[1] = 10;
    pthread_t thread_index;
    pthread_create (&thread_index, NULL, failing_thread_function, (void*)argument);

    void *retval;
    pthread_join (thread_index, &retval);

    return (uintptr_t)retval;
}

/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 4 at 0x\[0-9a-f\]* tags: 00/\[\[:xdigit:\]\]\[\[:xdigit:\]\].* \\(ptr/mem\\) in thread T1.*" } */
/* { dg-output "Address 0x\[0-9a-f\]* is located in stack of thread T0.*" } */
/* { dg-output "SUMMARY: HWAddressSanitizer: tag-mismatch \[^\n\]*.*" } */
