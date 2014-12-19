/* { dg-do compile } */
extern void abort (void);
void f(void*p,...){}
void g(void*p,long a,long b){if (a!=8) abort();}
