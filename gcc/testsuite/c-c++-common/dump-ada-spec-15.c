/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

extern void (*signal (int __sig, void (*__handler)(int)))(int);

/* { dg-final { cleanup-ada-spec } } */
