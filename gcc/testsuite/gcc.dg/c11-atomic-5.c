/* PR c/82679 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */

typedef _Atomic int A[10];
A a;

typedef _Atomic int I;
typedef I T[10];
T t;
