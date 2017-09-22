/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-ifunc "" }  */

__attribute__((target("default")))
int foo(void) { return 1; }
__attribute__((target("128bit-long-double")))
int foo(void) { return 1; }
__attribute__((target("80387")))
int foo(void) { return 1; }
__attribute__((target("96bit-long-double")))
int foo(void) { return 1; }
__attribute__((target("long-double-80")))
int foo(void) { return 1; }
__attribute__((target("long-double-64")))
int foo(void) { return 1; }
__attribute__((target("accumulate-outgoing-args")))
int foo(void) { return 1; }
__attribute__((target("align-double")))
int foo(void) { return 1; }
__attribute__((target("align-stringops")))
int foo(void) { return 1; }
__attribute__((target("fancy-math-387")))
int foo(void) { return 1; }
__attribute__((target("force-drap")))
int foo(void) { return 1; }
__attribute__((target("fp-ret-in-387")))
int foo(void) { return 1; }
__attribute__((target("hard-float")))
int foo(void) { return 1; }
__attribute__((target("ieee-fp")))
int foo(void) { return 1; }
__attribute__((target("inline-all-stringops")))
int foo(void) { return 1; }
__attribute__((target("inline-stringops-dynamically")))
int foo(void) { return 1; }
__attribute__((target("intel-syntax")))
int foo(void) { return 1; }
__attribute__((target("ms-bitfields")))
int foo(void) { return 1; }
__attribute__((target("no-align-stringops")))
int foo(void) { return 1; }
__attribute__((target("no-fancy-math-387")))
int foo(void) { return 1; }
__attribute__((target("no-push-args")))
int foo(void) { return 1; }
__attribute__((target("no-red-zone")))
int foo(void) { return 1; }
__attribute__((target("omit-leaf-frame-pointer")))
int foo(void) { return 1; }
__attribute__((target("pc32")))
int foo(void) { return 1; }
__attribute__((target("pc64")))
int foo(void) { return 1; }
__attribute__((target("pc80")))
int foo(void) { return 1; }
__attribute__((target("push-args")))
int foo(void) { return 1; }
__attribute__((target("red-zone")))
int foo(void) { return 1; }
__attribute__((target("rtd")))
int foo(void) { return 1; }
__attribute__((target("soft-float")))
int foo(void) { return 1; }
__attribute__((target("sseregparm")))
int foo(void) { return 1; }
__attribute__((target("stackrealign")))
int foo(void) { return 1; }
__attribute__((target("stack-arg-probe")))
int foo(void) { return 1; }
__attribute__((target("tls-direct-seg-refs")))
int foo(void) { return 1; }
__attribute__((target("vect8-ret-in-mem")))
int foo(void) { return 1; }
__attribute__((target("recip")))
int foo(void) { return 1; }
__attribute__((target("cld")))
int foo(void) { return 1; }
__attribute__((target("vzeroupper")))
int foo(void) { return 1; }
__attribute__((target("dispatch-scheduler")))
int foo(void) { return 1; }
__attribute__((target("prefer-avx128")))
int foo(void) { return 1; }
__attribute__((target("prefer-avx256")))
int foo(void) { return 1; }
__attribute__((target("32")))
int foo(void) { return 1; }
__attribute__((target("64")))
int foo(void) { return 1; }
__attribute__((target("x32")))
int foo(void) { return 1; }
__attribute__((target("mmx")))
int foo(void) { return 1; }
__attribute__((target("3dnow")))
int foo(void) { return 1; }
__attribute__((target("3dnowa")))
int foo(void) { return 1; }
__attribute__((target("sse")))
int foo(void) { return 1; }
__attribute__((target("sse2")))
int foo(void) { return 1; }
__attribute__((target("sse3")))
int foo(void) { return 1; }
__attribute__((target("ssse3")))
int foo(void) { return 1; }
__attribute__((target("sse4.1")))
int foo(void) { return 1; }
__attribute__((target("sse4.2")))
int foo(void) { return 1; }
__attribute__((target("sse4")))
int foo(void) { return 1; }
__attribute__((target("no-sse4")))
int foo(void) { return 1; }
__attribute__((target("sse5")))
int foo(void) { return 1; }
__attribute__((target("avx")))
int foo(void) { return 1; }
__attribute__((target("avx2")))
int foo(void) { return 1; }
__attribute__((target("fma")))
int foo(void) { return 1; }
__attribute__((target("sse4a")))
int foo(void) { return 1; }
__attribute__((target("fma4")))
int foo(void) { return 1; }
__attribute__((target("xop")))
int foo(void) { return 1; }
__attribute__((target("lwp")))
int foo(void) { return 1; }
__attribute__((target("abm")))
int foo(void) { return 1; }
__attribute__((target("popcnt")))
int foo(void) { return 1; }
__attribute__((target("bmi")))
int foo(void) { return 1; }
__attribute__((target("bmi2")))
int foo(void) { return 1; }
__attribute__((target("lzcnt")))
int foo(void) { return 1; }
__attribute__((target("hle")))
int foo(void) { return 1; }
__attribute__((target("rdseed")))
int foo(void) { return 1; }
__attribute__((target("prfchw")))
int foo(void) { return 1; }
__attribute__((target("adx")))
int foo(void) { return 1; }
__attribute__((target("fxsr")))
int foo(void) { return 1; }
__attribute__((target("xsave")))
int foo(void) { return 1; }
__attribute__((target("xsaveopt")))
int foo(void) { return 1; }
__attribute__((target("tbm")))
int foo(void) { return 1; }
__attribute__((target("cx16")))
int foo(void) { return 1; }
__attribute__((target("sahf")))
int foo(void) { return 1; }
__attribute__((target("movbe")))
int foo(void) { return 1; }
__attribute__((target("crc32")))
int foo(void) { return 1; }
__attribute__((target("aes")))
int foo(void) { return 1; }
__attribute__((target("pclmul")))
int foo(void) { return 1; }
__attribute__((target("sse2avx")))
int foo(void) { return 1; }
__attribute__((target("fsgsbase")))
int foo(void) { return 1; }
__attribute__((target("rdrnd")))
int foo(void) { return 1; }
__attribute__((target("f16c")))
int foo(void) { return 1; }
__attribute__((target("fentry")))
int foo(void) { return 1; }
__attribute__((target("8bit-idiv")))
int foo(void) { return 1; }
__attribute__((target("avx256-split-unaligned-load")))
int foo(void) { return 1; }
__attribute__((target("avx256-split-unaligned-store")))
int foo(void) { return 1; }
__attribute__((target("rtm")))
int foo(void) { return 1; }
//---------------

#include <stdio.h>
    int main (void)
    {
      int result;
      result = foo();
      printf("Result is %d\n", result);
      return result;
    }

/* { dg-prune-output "attribute.* is unknown" } */
/* { dg-prune-output "missing 'target' attribute*" } */
/* { dg-prune-output "redefinition of 'int foo" } */
/* { dg-prune-output "No dispatcher found for" } */
