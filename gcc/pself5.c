#include <stdio.h>
#define N '\n'
#define Q '\"'
#define B '\\'
int main(void){char*p="#include <stdio.h>%c#define N '%cn'%c#define Q '%c%c'%c#define B '%c%c'%cint main(void){char*p=%c%s%c;(void)printf(p,N,B,N,B,Q,N,B,B,N,Q,p,Q,N);return 0;}%c";(void)printf(p,N,B,N,B,Q,N,B,B,N,Q,p,Q,N);return 0;}
