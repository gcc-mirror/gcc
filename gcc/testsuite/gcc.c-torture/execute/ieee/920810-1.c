#include <stdio.h>
double normalize(x)double x;{if(x==0)x=0;return x;}
main(){char b[9];sprintf(b,"%g",normalize(-0.0));if(strcmp(b,"0"))abort();exit(0);}
