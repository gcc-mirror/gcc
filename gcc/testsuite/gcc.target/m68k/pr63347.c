/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=5208 -w" } */

void __attribute__ ((noinline))
oof(const char *s)
{
  asm volatile ("" ::: "memory");
}
int print_info(unsigned int *ip_addr)
{
    int invalid = 0;

    if (ip_addr) {
        unsigned int haddr = *ip_addr;
        oof("stuff");
        if (0x0 == haddr) {
            invalid = 1;
        }
        oof("stuff2");
    } else {
        invalid = 1;
    }

    return invalid;
}

int main(int argc, char *argv[])
{
    unsigned int myaddr;
    int ret;

    myaddr = 0x0;
    ret = print_info(&myaddr);
    if (!ret)
        __builtin_abort ();

    myaddr = 0x01020304;
    ret = print_info(&myaddr);
    if (ret)
        __builtin_abort ();
    __builtin_exit (0);
}


