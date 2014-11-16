extern int nc;
void f(void)
{
    unsigned char resp[1024];
    int c;
    int bl = 0;
    unsigned long long *dwords = (unsigned long long *)(resp + 5);
    for (c=0; c<nc; c++)
    {
        ff(dwords[bl/64]);
        bl++;
    }
}
