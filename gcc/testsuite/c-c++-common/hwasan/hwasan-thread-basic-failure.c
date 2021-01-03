/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */
/* { dg-additional-options "-lpthread" } */

/* Ensure the failure mode for hwasan under pthreads looks sane.
   (Looks sane means that the same error message is printed out rather than an
   opaque message due to mishandling).  */

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

void *
failing_from_stack (void * argument)
{
    int internal_array[16] = {0};
    printf ("(now should fail):");
    printf (" problem number is %d\n", internal_array[17]);
    return (void *)1;
}

int
main (int argc, char **argv)
{
    int argument[100] = {0};
    argument[1] = 10;
    pthread_t thread_index;
    pthread_create (&thread_index, NULL, failing_from_stack, (void*)argument);

    void *retval;
    pthread_join (thread_index, &retval);

    return (uintptr_t)retval;
}

/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 4 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/00 \\(ptr/mem\\) in thread T1.*" } */
/* { dg-output "Address 0x\[0-9a-f\]* is located in stack of thread T1.*" } */
/* { dg-output "SUMMARY: HWAddressSanitizer: tag-mismatch \[^\n\]*.*" } */
