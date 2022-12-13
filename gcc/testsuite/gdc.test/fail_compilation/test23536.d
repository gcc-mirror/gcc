/* TEST_OUTPUT:
---
fail_compilation/test23536.d(104): Error: function `test23536.S.nonctor` cannot be a non-static member function for `pragma(crt_constructor)`
fail_compilation/test23536.d(106): Error: function `test23536.S.nondtor` cannot be a non-static member function for `pragma(crt_destructor)`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23536

#line 100

struct S
{
    int x;
    extern (C) pragma(crt_constructor)        void nonctor() { } // should not compile
    extern (C) pragma(crt_constructor) static void stactor() { } // should compile
    extern (C) pragma(crt_destructor)         void nondtor() { } // should not compile
    extern (C) pragma(crt_destructor)  static void stadtor() { } // should compile
}
