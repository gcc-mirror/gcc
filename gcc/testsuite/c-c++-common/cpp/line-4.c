int line1;
int f = bob;
int b = bill();
int line4;

// { dg-do preprocess }
// { dg-options "-dD -include $srcdir/c-c++-common/cpp/line-4.h -nostdinc" }

// { dg-final { scan-file line-4.i {# 0 "[^\n]*/line-4.c"\n# 0 "<built-in>"\n} } }
// { dg-final { scan-file line-4.i {# 0 "<command-line>"\n# 1 "[^\n]*/line-4.h" 1\n#define bob 1\n} } }
// { dg-final { scan-file line-4.i {#define bill\(\) 2\n# 0 "<command-line>" 2\n# 1 "[^\n]*/line-4.c"\nint line1;\n} } }
