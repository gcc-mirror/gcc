

#if defined( AUX_ASM_CHECK )
#if !defined(NOINLINE) && !defined(__GNUC__) /* ain't got no inline, so we got it */
#endif /* NOINLINE */
#endif  /* AUX_ASM_CHECK */


#if defined( HPUX_MAXINT_CHECK )
#ifndef MAXINT
#define MAXINT 0x7FFFFFFF
#endif
#endif  /* HPUX_MAXINT_CHECK */
