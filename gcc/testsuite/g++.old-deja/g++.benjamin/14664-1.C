// 981203 bkoz
// g++/14664 - test
// Build don't link: 
// Special g++ Options: -fconst-strings

char foo[26];

void bar()
{
  //-g++: incompatible types in assignment of 'const char[]' to 'char[]'
  //-edg: expression must be a modifiable lvalue
  foo = "0123456789012345678901234"; // ERROR - // ERROR -
}



