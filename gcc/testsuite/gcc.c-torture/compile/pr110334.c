int n;
typedef void (*fnptr)();
fnptr get_me();
__attribute__ ((always_inline))
inline void test(void)
{
        if (n < 10)
          (get_me())();
        n++;
        return;
}
fnptr get_me()
{
        return test;
}
void
foo()
{
        test();
}
