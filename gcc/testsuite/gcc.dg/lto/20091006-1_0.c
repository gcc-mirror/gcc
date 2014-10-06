/* { dg-lto-do link } */

typedef void (*fnt) (void);
void __attribute__((noinline)) bar (void) {}
extern inline void __attribute__((gnu_inline)) check3 (void)
{
    bar ();
}
void test (void) 
{
    const fnt pcheck3 = check3;
    pcheck3 ();
}
int main() { return 0; }
