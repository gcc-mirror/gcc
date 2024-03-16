/* { dg-additional-options "-std=gnu89" } */

typedef struct{unsigned b0:1;}*t;x(a,b)t a,b;{b->b0=a->b0;}
