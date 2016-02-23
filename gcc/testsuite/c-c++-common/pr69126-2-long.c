/* { dg-options "-Wdeprecated-declarations" } */

/* The macro's name is >= 32 characters long, and hence its location
   requires an ad-hoc location.  */

#define IGNORE_WHERE_MACRO_IS_LONGER_THAN_31_CHARS  _Pragma("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
__attribute__((deprecated)) void f();
int main() {
    IGNORE_WHERE_MACRO_IS_LONGER_THAN_31_CHARS
    f();
}
