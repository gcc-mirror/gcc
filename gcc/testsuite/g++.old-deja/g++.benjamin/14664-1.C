// { dg-do assemble  }
// 981203 bkoz
// g++/14664 - test

char foo[26];

void bar()
{
  //-g++: incompatible types in assignment of 'const char[]' to 'char[]'
  //-edg: expression must be a modifiable lvalue
  foo = "0123456789012345678901234"; // { dg-error "" } // ERROR -
}



