/* { dg-additional-options "-std=gnu89" } */
typedef struct{int s;}S;foo(){int i=(int)&(S){(void*)((int)&(S){1})};}
