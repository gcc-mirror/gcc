// { dg-do assemble  }
// { dg-options "-fpermissive -w" }
// 981203 bkoz
// g++/14664 + test

char foo[26];

void bar()
{
  // the addition of the flag "-fno-const-string-literal" reverts to pre-ISO.
  // -g++: ANSI C++ forbids assignment of arrays
  foo = "0123456789012345678901234"; // WARNING -
}



