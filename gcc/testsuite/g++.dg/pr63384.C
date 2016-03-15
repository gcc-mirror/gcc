/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling2 -fsel-sched-pipelining  -fsel-sched-pipelining-outer-loops -fsel-sched-reschedule-pipelined -fvar-tracking-assignments-toggle -ftree-vectorize" } */

template <class T> T **make_test_matrix() {
 T **data = new T *;
 for (int i = 0; i < 1000; i++)
    ;
}

template <typename T> void test() { T **c = make_test_matrix<T>(); }

main() { test<float>(); }
