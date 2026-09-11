/* { dg-do run } */
/* { dg-additional-options "-march=rv64gc_zbb_zbs -mabi=lp64d" { target rv64 } } */ 
/* { dg-additional-options "-march=rv32gc_zbb_zbs -mabi=ilp32" { target rv32 } } */ 

typedef unsigned short uint16_t;

__attribute__((noinline)) unsigned
scan(uint16_t mask)
{
    unsigned result = 0, dword = mask, bit = 0;

    while (dword) {
        result |= 1u << bit;
	dword &= dword - 1;
	bit = __builtin_ffs(dword) - 1;
    }

    return result;
}

int
main(void)
{
    const unsigned expected = 0xffff;
    const unsigned actual = scan(expected);

    if (actual != expected)
      __builtin_abort ();
    __builtin_exit (0);
}
