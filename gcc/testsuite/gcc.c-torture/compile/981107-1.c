void call(void);
unsigned long seed(void)
{
    unsigned long u;

    call();

    u = 26107 * (unsigned long)&u;
    return u;
}
