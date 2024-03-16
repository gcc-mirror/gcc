/* { dg-require-stack-size "40000 * 4 + 256" } */

void exit (int);

int main(void){int d[40000];d[0]=0;exit(0);}

