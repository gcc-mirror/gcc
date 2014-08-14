/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-add-options bind_pic_locally } */

typedef struct A { int a,b; } A;
int*g(int*x){return x;}
int*f1(){
  A x[2]={{1,2},{3,4}};
  return g(&x[1].a); // { dg-warning "returns address of local variable" }
}
int*f2(int n){
  A x[2]={{1,2},{3,4}};
  return n?0:g(&x[1].a); // { dg-warning "may return address of local variable" }
}
A y[2]={{1,2},{3,4}};
int*h(){
  return g(&y[1].a);
}
int*j(int n){
  A x[2]={{1,2},{3,4}};
  int*p=g(&y[1].a);
  if(n==1)p=g(&x[1].a);
  if(n==2)p=g(&x[0].b);
  return p; // { dg-warning "may return address of local variable" }
}
int*s()
{
  static int i;
  return &i;
}
