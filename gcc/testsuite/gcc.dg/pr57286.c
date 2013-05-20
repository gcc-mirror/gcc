/* { dg-do compile } */
/* { dg-options "-O" } */

typedef int vec __attribute__ ((vector_size (4*sizeof(int))));
void f (vec *x){
    *x = (*x < 0) | 1;
}
