

#if defined( HP_SYSFILE_CHECK )
extern void foo(struct file *, ...); /* HPUX_SOURCE - bad varargs */
#endif  /* HP_SYSFILE_CHECK */
