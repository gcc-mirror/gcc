#define STACK_REQUIREMENT (40000 * 4 + 256)
#if defined (STACK_SIZE) && STACK_SIZE < STACK_REQUIREMENT
main () { exit (0); }
#else
main(){int d[40000];d[0]=0;exit(0);}
#endif
