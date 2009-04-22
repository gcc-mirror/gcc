/* { dg-do compile } */

/* PR C/31499, test that the C front-end treats vectors like an array. */

#define vector __attribute__((__vector_size__(4*sizeof(int)) ))
vector signed int v1[]={0,1,2,3,4,5,6,7};
