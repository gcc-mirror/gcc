// { dg-options "" }
// PR c++/16193

# 1 "syshdr1.C"
# 1 "syshdr1.h" 1 3
// Redefinitions of built-in types are allowed in system headers so
// that G++ will work with system headers that are not fully
// C++-aware.
typedef long wchar_t;
# 2 "syshdr1.C" 2
