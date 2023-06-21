/* { dg-lto-do run } */
/* { dg-lto-options { { -flto -O2 -finline-limit=150 } } } */

[[gnu::noipa]]
void hjj (unsigned int lk)
{
    (void)lk;
}
void nn(int i, int n);
[[gnu::noinline]]
int ll(void) {
    return 1;
}
void hh(int* dest, int src)
{
    if (!ll() && !src)
        hjj(100);
    (*dest) = 1;
}
void gg(int* result, int x)
{
    if (x >= 0)
        return;

    int xx;
    xx = *result;
    hh(result, ll());
    if (xx >= *result)
        nn(xx, *result);
}
void nn(int i, int n) {
    int T8_;
    if (n < 0)
        __builtin_exit(0);
    T8_ = 0;
    gg(&T8_, i);
    __builtin_exit(0);
}
void kk(int* x, int i) {
    hh(x, ll());
    if (i < 0 || i >= *x)
        nn(i,*x);
}
int g__r_1 = 0;
int main() {
    kk(&g__r_1, 0);
    return 0;
}
