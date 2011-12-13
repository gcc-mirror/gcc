/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O1" } */

static inline void asmfunc()
{
__asm__("");
}

__attribute__((transaction_callable))
void push()
{
        asmfunc();
}
