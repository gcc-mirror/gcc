/* { dg-do run } */
/* { dg-require-effective-target nonlocal_goto } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target trampolines } */

extern void exit (int);
extern void abort (void);
int s(int i){if(i>0){__label__ l1;int f(int i){if(i==2)goto l1;return 0;}return f(i);l1:;}return 1;}
int x(){return s(0)==1&&s(1)==0&&s(2)==1;}
int main(){if(x()!=1)abort();exit(0);}
