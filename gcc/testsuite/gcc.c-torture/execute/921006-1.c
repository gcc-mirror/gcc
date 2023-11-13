/* REPRODUCED:RUN:SIGNAL MACHINE:i386 OPTIONS:-O */
void abort(void);
void exit(int);
int
main(void)
{
if(__builtin_strcmp("X","")<0)abort();
exit(0);
}
