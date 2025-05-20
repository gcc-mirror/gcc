// PR c++/118904
// { dg-module-do run }
// { dg-additional-options "-fmodules -std=c++20 -fdump-lang-module-uid" }

import "src-loc-1_a.H";

int main() {
  const char* a = foo().function_name();
  const char* b = std::source_location::current().function_name();
  if (__builtin_strcmp(a, "std::source_location foo()"))
    __builtin_abort();
  if (__builtin_strcmp(b, "int main()"))
    __builtin_abort();
}

// { dg-final { scan-lang-dump {Read internal identifier:-[0-9]* [^\n\r]*Lsrc_loc} module } }
