/* { dg-do run } */
/* { dg-options "-O3" } */

__attribute__((noinline))
static int local_f(int a, int b) { return a + b; }

__attribute__((noinline))
int extern_f(int a, int b) { return local_f(a, b); }

int  unused_g1(int b) { return extern_f(0, b); }

int (*volatile fp)(int, int) = extern_f;

int main() { if (fp(1, 2) != 3) __builtin_abort(); }
