/* Test C2x UTF-8 characters.  Test not accepted for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define z(x) 0
#define u8 z(
unsigned char a = u8'a');
