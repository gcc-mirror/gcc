/* { dg-require-effective-target label_values } */

void f(void){static void*t[]={&&x};x:;}
void g(void){static unsigned p[5];}
