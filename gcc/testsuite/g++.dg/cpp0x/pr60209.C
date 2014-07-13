// PR c++/60209
// { dg-do compile { target c++11 } }

// http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_defects.html#1473

void operator "" "" _x(unsigned long long);

void operator "" "" "" _x(unsigned long long);

void operator "" ""_w(unsigned long long);

void operator "" ""_w ""(unsigned long long);
