/* { dg-additional-options "-std=gnu89" } */

x(){signed char c=-1;return c<0;}main(){if(x()!=1)abort();exit(0);}
