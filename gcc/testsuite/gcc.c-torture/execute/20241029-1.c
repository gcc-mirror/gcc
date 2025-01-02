/* PR rtl-optimization/117327 */
/* Testcase by Brad Moody <brad.moody@oracle.com> */

__attribute__((noinline))
void foo(int *self, int *x)
{
    __builtin_puts ("foo\n");

    if (x) {
        while (1) {
            ++*self;
            if (*self == 6) break;
            if (*self == 7) __builtin_unreachable();
        }
    }
}

int main (void)
{
    int y = 0;
    foo (&y, 0);
    return 0;
}
