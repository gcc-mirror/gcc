/* REPRODUCED:RUN:SIGNAL MACHINE:sparc OPTIONS: */
void exit (int);
void
f(int n)
{
int i;
double v[n];
for(i=0;i<n;i++)
v[i]=0;
}
int
main(void)
{
f(100);
exit(0);
}
