/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

extern void (*signal (int __sig, void (*__handler)(int)))(int);

/* { dg-final { scan-ada-spec "uu_sig" } } */
/* { dg-final { scan-ada-spec "uu_handler" } } */
/* { dg-final { cleanup-ada-spec } } */
