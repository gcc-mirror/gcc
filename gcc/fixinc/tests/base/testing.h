

#if defined( CTRL_QUOTES_DEF_CHECK )
#define _CTRL(c) (c&037)
#endif  /* CTRL_QUOTES_DEF_CHECK */


#if defined( CTRL_QUOTES_USE_CHECK )
#define TCTRLFOO BSD43_CTRL('T', 1)
#endif  /* CTRL_QUOTES_USE_CHECK */


#if defined( IO_QUOTES_DEF_CHECK )
#define XX_IO(x)        (x<<8|256)
#endif  /* IO_QUOTES_DEF_CHECK */


#if defined( IO_QUOTES_USE_CHECK )
#define TIOCFOO BSD43__IOWR('T', 1)
#define TIOCFOO \
BSD43__IOWR('T', 1) /* Some are multi-line */
#endif  /* IO_QUOTES_USE_CHECK */


#if defined( MACHINE_NAME_CHECK )
/* MACH_DIFF: */
#if defined( __i386__ ) || defined( sparc ) || defined( vax )
/* no uniform test, so be careful  :-) */
#endif  /* MACHINE_NAME_CHECK */


#if defined( SCO_STRICT_ANSI_CHECK )
#if !defined(__STRICT_ANSI__) /* not standard C */
int foo;
#endif
#endif  /* SCO_STRICT_ANSI_CHECK */


#if defined( UNDEFINE_NULL_CHECK )
#ifndef NULL
#define NULL 0UL
#endif
#ifndef NULL
#define NULL	((void*)0)
#endif

#endif  /* UNDEFINE_NULL_CHECK */
