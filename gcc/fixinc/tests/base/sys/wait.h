

#if defined( AIX_SYSWAIT_CHECK )
/* bos325, */
struct rusage;
extern pid_t wait3();
	/* pid_t wait3(int *, int, struct rusage *); */
#endif  /* AIX_SYSWAIT_CHECK */


#if defined( NEXT_WAIT_UNION_CHECK )
extern pid_d wait(void*);
#endif  /* NEXT_WAIT_UNION_CHECK */
