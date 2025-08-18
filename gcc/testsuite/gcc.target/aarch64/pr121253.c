/* { dg-options "-O" } */

struct s128 {
    long a, b;
};

struct s128 foo(void) {
    struct s128 ret;
    asm("mov %0, #0 \n\t"
        "mov %R0, #0 \n\t"
        "mov x0, #12345"
        : "=r" (ret) : : "x0");
    return ret;
}

/* { dg-final { scan-assembler-not {mov x0, #0} } } */
