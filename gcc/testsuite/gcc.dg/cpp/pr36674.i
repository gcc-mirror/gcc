/* PR cpp/36674  #include location is offset by one row in errors from preprocessed files */
/* { dg-do compile } */
/* { dg-options "-fshow-column" } */
# 1 "gcc/testsuite/gcc.dg/pr36674.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "gcc/testsuite/gcc.dg/pr36674.c"
# 1 "gcc/testsuite/gcc.dg/pr36674.h" 1
not_declared_yet();
# 1 "gcc/testsuite/gcc.dg/pr36674.c" 2
/* { dg-message "file included from \[^\n\]*pr36674.c:1:" "correct include line" { target *-*-* } 0 } */
/* { dg-message "pr36674.h:1:1: warning: data definition has no type or storage class" "correct warning" { target *-*-* } 0 } */
