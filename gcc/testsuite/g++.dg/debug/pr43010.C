// PR debug/43010
// { dg-do compile }
// { dg-options "-g -femit-struct-debug-baseonly" }
# 1 "foo.C"
# 1 "bar.h" 1
typedef struct { int i; } S __attribute__((aligned));
typedef struct { struct { int i; } j; } T __attribute__((aligned));
# 1 "foo.C" 2
