// { dg-do assemble  }
// { dg-options "" }
// Test that we don't complain about trying to define bool or wchar_t in a
// system header.


# 1 "syshdr1.C"
# 1 "syshdr1.h" 1 3
typedef int bool;
typedef int wchar_t;
# 2 "syshdr1.C" 2
