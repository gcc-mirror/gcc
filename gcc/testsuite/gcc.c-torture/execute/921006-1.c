/* REPRODUCED:RUN:SIGNAL MACHINE:i386 OPTIONS:-O */
void abort(void);
void exit(int);
main()
{
if(strcmp("X","")<0)abort();
exit(0);
}
