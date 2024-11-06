# 0 ".../line-1.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "./line-2.h" 1
#define bob 1

#define bill() 2
#error wrong
# 0 "<command-line>" 2
# 1 ".../line-3.c"
int line1;
int f = bob;
int bill(1);
int line4;

// { dg-regexp {In file included from <command-line>:\n[^\n]*/line-2.h:4:2: error: #error wrong\n} }

// { dg-regexp {[^\n]*/line-3.c:3:11: error: macro 'bill' passed 1 arguments, but takes just 0\n[^\n]*/line-2.h:3:9: note: macro 'bill' defined here\n} }

// { dg-options "-fpreprocessed -fdirectives-only" }
