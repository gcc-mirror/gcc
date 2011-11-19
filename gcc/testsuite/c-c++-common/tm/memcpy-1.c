/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */
#include <string.h>

__attribute__((transaction_safe))
void *wmemcpy(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
}
