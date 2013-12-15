/* Test for cross x86_64<->w64 abi standard calls via variable.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -ffast-math -maccumulate-outgoing-args" } */
#include "callabi.h"

extern void abort (void);

typedef int (CALLABI_CROSS *func)(void *, char *, char *, short, long long);

int CALLABI_CROSS
callback(void * ptr, char *string1, char *string2, short number, long long rand)
{
    return (rand != 0x1234567890abcdefLL);
}

int main()
{
    volatile func callme = callback;
    if(callme(0, 0, 0, 0, 0x1234567890abcdefLL))
     abort();
    return 0;
}
