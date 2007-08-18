/* PR preprocessor/32974 - don't warn for extra tokens in pragma dependency */
/* { dg-do compile } */

#pragma GCC dependency "pr32974.c" extra tokens are ok

int x;
