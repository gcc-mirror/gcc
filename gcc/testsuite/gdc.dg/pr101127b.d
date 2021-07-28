// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101127
// { dg-do compile { target i?86*-*-* x86_64-*-* } }

import gcc.builtins;

static assert(!__traits(compiles, __builtin_ia32_andps256));
static assert(!__traits(compiles, __builtin_ia32_pmulhrsw128));
