int line1;
int f = bob;
int bill(1);
int line4;

// { dg-do preprocess }
// { dg-options "-dD -include $srcdir/c-c++-common/cpp/line-2.h -nostdinc" }

// { dg-regexp {In file included from <command-line>:\n[^\n]*/line-2.h:4:2: error: #error wrong\n} }

// { dg-regexp {[^\n]*/line-2.c:3:11: error: macro 'bill' passed 1 arguments, but takes just 0\n[^\n]*/line-2.h:3:9: note: macro 'bill' defined here\n} }
