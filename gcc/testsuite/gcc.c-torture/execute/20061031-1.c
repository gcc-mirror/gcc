/* PR rtl-optimization/29631 */
/* Origin: Falk Hueffner <falk@debian.org> */

const signed char nunmap[] = { 17, -1, 1 };

__attribute__((noinline))
void ff(int i) {
    asm volatile("");
}

__attribute__((noinline))
void f(short delta)
{
    short p0 = 2, s;
    for (s = 0; s < 2; s++)
    {
        p0 += delta;
        ff(s);
        if (nunmap[p0] == 17)
            asm volatile("");
    }
}

int main(void)
{
    f(-1);
    return 0;
}
