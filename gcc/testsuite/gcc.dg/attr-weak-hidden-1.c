/* { dg-do run } */
/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2" } */
/* { dg-additional-sources "attr-weak-hidden-1a.c" } */
int __attribute__((visibility("hidden"))) foo (void) { return 0; }
