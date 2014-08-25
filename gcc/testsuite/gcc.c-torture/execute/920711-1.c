/* { dg-options "-fwrapv" } */

extern void abort (void);
extern void exit (int);

f(long a){return (--a > 0);}
main(){if(f(0x80000000L)==0)abort();exit(0);}
