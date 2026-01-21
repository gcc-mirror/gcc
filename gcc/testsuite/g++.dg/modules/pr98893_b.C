// { dg-additional-options "-fmodules" }
// { dg-require-effective-target cxa_atexit }
// { dg-additional-options "-fuse-cxa-atexit" }

import "pr98893_a.H";
static S b[1];
int main() {
  foo();
}

// { dg-final { scan-assembler {__tcf_ZZ3foovE1a:} } }
// { dg-final { scan-assembler {__tcf_ZL1b:} { xfail hppa*-*-hpux* } } }
