// { dg-do assemble  }
// { dg-options "-fpermissive -w" }
// 981203 bkoz
// g++/14664 + test

char foo[26];

void bar()
{
  foo = "0123456789012345678901234"; // { dg-error "array" }
}



