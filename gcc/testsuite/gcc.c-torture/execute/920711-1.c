/* { dg-options "-fwrapv" } */

extern void abort (void);
extern void exit (int);

int f(long a){return (--a > 0);}
int main(){if(f(0x80000000L)==0)abort();exit(0);}
