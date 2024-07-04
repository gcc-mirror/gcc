/* { dg-additional-options "-std=gnu89" } */
f(char*c){extern char a[],b[];return a+(b-c);}
