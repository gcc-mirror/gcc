/* { dg-do run } */
/* { dg-options "-O -fwrapv" } */

#define DEF(num, T1, T2) T2 f##num(T1 x) { \
    T1 y = -x; \
    T2 z = (T2)y; \
    return -z; \
}
DEF(0, int, long long)

int main(){
    volatile int a = -1 - __INT_MAX__;
    volatile long long b = f0 (a);
    volatile long long c = a;
    volatile long long d = -c;
    if (b != d)
      __builtin_abort();
}
