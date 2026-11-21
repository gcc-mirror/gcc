/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-Os -fdump-tree-optimized" } */
/* { dg-additional-options "-mpopcnt" { target { x86_64-*-* } } } */
/* { dg-require-effective-target int32plus } */

/* Narrowed: masked value fits in 32 bits.  */
int func1(unsigned long long x) {
    return __builtin_popcountll(x & 0x87878787);
}

/* Narrowed: x >> 32 leaves only 32 bits.  */
int func2(unsigned long long x) {
    return __builtin_popcountll(x >> 32);
}

/* Narrowed: constant shifted right stays below 2^32.  */
int func3(unsigned char x) {
    if (x >= 64) {
      __builtin_unreachable();
    }
    return __builtin_popcountll(0x87878787ULL >> x);
}

/* Narrowed: remainder by a 32-bit constant is below 2^32.  */
int func4(unsigned long long x) {
    return __builtin_popcountll(x % 4294967291U);
}

/* Narrowed: quotient by a constant above 2^32 is below 2^32.  */
int func5(unsigned long long x) {
    return __builtin_popcountll(x / 4294967311U);
}

/* Narrowed: __builtin_unreachable limits x to 32 bits.  */
int func6(unsigned long long x) {
    if (x > 0xFFFFFFFFULL) {
      __builtin_unreachable();
    }
    return __builtin_popcountll(x);
}

/* Narrowed: signed x known to be in [0, 0xFFFFFFFF].  */
int func7(long long x) {
    if (x < 0 || x > 0xFFFFFFFFLL)
      __builtin_unreachable();
    return __builtin_popcountll(x);
}

/* Not narrowed: x is in [0, LLONG_MAX], which needs 63 bits.  */
int func8(long long x) {
    if (x < 0)
      __builtin_unreachable();
    return __builtin_popcountll(x);
}

/* Not narrowed: no range information.  */
int func9(unsigned long long x) {
    return __builtin_popcountll(x);
}

/* Not narrowed: x <= 2^32 needs 33 bits.  */
int func10(unsigned long long x) {
    if (x > 0x100000000ULL)
      __builtin_unreachable();
    return __builtin_popcountll(x);
}

/* The 7 narrowable cases (func1-func7) call a 32-bit .POPCOUNT.  */
/* { dg-final { scan-tree-dump-times {\.POPCOUNT \(_[0-9]+\)} 7 "optimized" } } */

/* Only func8, func9 and func10 keep the long long builtin.  */
/* { dg-final { scan-tree-dump-times {__builtin_popcountll \(} 3 "optimized" } } */

/* { dg-final { scan-tree-dump-not {__builtin_popcountll \(_[0-9]+\)} "optimized" } } */
