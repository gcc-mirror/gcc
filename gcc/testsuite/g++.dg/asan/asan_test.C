// { dg-do run { target { { i?86-*-linux* x86_64-*-linux* } && sse2_runtime } } }
// { dg-skip-if "" { *-*-* } { "*" } { "-O2" } }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }
// { dg-additional-sources "asan_globals_test-wrapper.cc" }
// { dg-options "-std=c++11 -fsanitize=address -fno-builtin -Wall -Wno-format -Werror -g -DASAN_UAR=0 -DASAN_HAS_EXCEPTIONS=1 -DASAN_HAS_BLACKLIST=0 -DSANITIZER_USE_DEJAGNU_GTEST=1 -lasan -lpthread -ldl" }
// { dg-additional-options "-DASAN_NEEDS_SEGV=1" { target { ! arm*-*-* } } }
// { dg-additional-options "-DASAN_LOW_MEMORY=1 -DASAN_NEEDS_SEGV=0" { target arm*-*-* } }
// { dg-additional-options "-DASAN_AVOID_EXPENSIVE_TESTS=1" { target { ! run_expensive_tests } } }
// { dg-additional-options "-msse2" { target { i?86-*-linux* x86_64-*-linux* } } }
// { dg-additional-options "-D__NO_INLINE__" { target { *-*-linux-gnu } } }
// { dg-final { asan-gtest } }

#include "asan_test.cc"
#include "asan_mem_test.cc"
#include "asan_str_test.cc"
#include "asan_oob_test.cc"
