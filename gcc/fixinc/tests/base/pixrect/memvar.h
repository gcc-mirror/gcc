

#if defined( SUN_CATMACRO_CHECK )
#ifdef __STDC__
#  define CAT(a,b) a##b
#else
#define CAT(a,b)	a/**/b
#endif
#endif  /* SUN_CATMACRO_CHECK */
