/* Test whether traditional stringify works.  */
/* { dg-do preprocess } */
/* { dg-options "-traditional" } */
#define foo(a, b) c="a"; d="b";
