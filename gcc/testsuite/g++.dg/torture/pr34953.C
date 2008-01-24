/* { dg-do compile } */
/* { dg-options "-w" } */

void B_CLEAR(void* ret);
void B_NeverReturns(void* ret) __attribute__((noreturn));

int main()
{
    const struct AutoErrPop { ~AutoErrPop() { } } AutoErrPopper = { };
    B_NeverReturns(0);
}

void B_NeverReturns(void* ret)
{
    B_CLEAR(ret); /* Never returns (does a setjmp/goto) */
}

