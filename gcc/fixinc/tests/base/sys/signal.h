

#if defined( AIX_VOLATILE_CHECK )
typedef int sig_atomic_t;
#endif  /* AIX_VOLATILE_CHECK */


#if defined( SUN_SIGNAL_CHECK )
#ifdef __cplusplus
void	(*signal(...))(...);
#else
void	(*signal())();
#endif
#endif  /* SUN_SIGNAL_CHECK */
