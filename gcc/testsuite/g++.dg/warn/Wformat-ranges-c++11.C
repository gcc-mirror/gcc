// { dg-do compile { target c++11 } }
// { dg-skip-if "char8_t" { c++2a } }
/* { dg-options "-Wformat -fdiagnostics-show-caret" } */

/* C++11-specific format tests. */

#define printf __builtin_printf

void test_u8 (const char *msg)
{
  printf(u8"hello %i", msg);/* { dg-warning "format '%i' expects argument of type 'int', but argument 2 has type 'const char\\*' " } */
/* { dg-begin-multiline-output "" }
   printf(u8"hello %i", msg);
                   ~^   ~~~
                    |   |
                    int const char*
                   %s
   { dg-end-multiline-output "" } */
}
