// { dg-do compile { target { { { i?86-*-* x86_64-*-* } && { ! ia32 } } || { powerpc*-*-* aarch64*-*-* riscv*-*-* }  } } }
// { dg-options "-O2  -fdump-rtl-pro_and_epilogue"  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

// PR rtl-optimization/118502

// The shrink-wrapping should happen around the slow path of vector<int>::push_back,
// The fast path is just checking if there is enough space and doing a few stores.
// We want to verify that shrink wrapping always happens.

#include <vector>

void push_back(std::vector<int>& xs, unsigned char x) {
    xs.push_back(x);
}

/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" } } */
