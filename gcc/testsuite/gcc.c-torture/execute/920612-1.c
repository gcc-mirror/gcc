/* { dg-options "-fwrapv" } */

extern void abort (void);
extern void exit (int);

f(j)int j;{return++j>0;}
main(){if(f((~0U)>>1))abort();exit(0);}
