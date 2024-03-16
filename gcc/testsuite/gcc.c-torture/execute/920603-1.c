/* { dg-additional-options "-std=gnu89" } */
f(got){if(got!=0xffff)abort();}
main(){signed char c=-1;unsigned u=(unsigned short)c;f(u);exit(0);}
