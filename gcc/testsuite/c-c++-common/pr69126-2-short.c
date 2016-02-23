/* { dg-options "-Wdeprecated-declarations" } */

/* IGNORE_SHORT_MACRO is < 32 characters long, and hence its location
   can be stored without needing an ad-hoc location.  */

#define IGNORE_SHORT_MACRO  _Pragma("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
__attribute__((deprecated)) void f();
int main() {
    IGNORE_SHORT_MACRO
    f();
}
