#include <stdint.h>

__attribute__((transaction_safe))
void foo() 
{
}

uint32_t _ITM_beginTransaction(uint32_t prop, ...)
{
}

void __builtin__ITM_commitTransaction (void)
{
}
