/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target trampolines } */
/* { dg-additional-options "-std=gnu89" } */

s(i){if(i>0){__label__ l1;int f(int i){if(i==2)goto l1;return 0;}return f(i);l1:;}return 1;}
x(){return s(0)==1&&s(1)==0&&s(2)==1;}
main(){if(x()!=1)abort();exit(0);}
