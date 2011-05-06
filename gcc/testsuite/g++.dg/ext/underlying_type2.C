// { dg-do compile }

enum E1 { };
enum E2 { a = -1, b = 1 };
enum E3 { c = __LONG_MAX__ };

__underlying_type(E1) e1 = 0;
__underlying_type(E2) e2 = b;
__underlying_type(E3) e3 = __LONG_MAX__;
