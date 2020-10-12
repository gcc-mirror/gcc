/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

int f(){ int a; return &a==(void *)"hello"; }
int g(){ return "bye"=="hello"; }
int h() { return "bye"=="helloooobye"+8; }

/* { dg-final { scan-tree-dump-times "hello" 1 "original" } } */
/* The test in h() should be retained because the result depends on
   string merging.  */
/* { dg-final { scan-assembler "hellooo" { target { ! nvptx*-*-* } } } } */
/* { dg-final { scan-assembler "104, 101, 108, 108, 111, 111, 111" { target { nvptx*-*-* } } } } */

