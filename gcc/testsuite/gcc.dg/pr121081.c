/* { dg-do compile } */ 
/* { dg-options "-std=c11" } */

int * _Atomic __attribute__((visibility(""))) a;	/* { dg-warning "attribute ignored" } */
int * _Atomic a;

int * _Atomic __attribute__((visibility("hidden"))) b;	/* { dg-warning "attribute ignored" } */
int * _Atomic b;

