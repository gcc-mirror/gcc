

#if defined( ARM_WCHAR_CHECK )
# ifndef 	 _GCC_WCHAR_T /* we don't have wchar_t yet, ... */
#  define  _GCC_WCHAR_T  short
# endif /* __wchar_t */
#endif  /* ARM_WCHAR_CHECK */


#if defined( INT_ABORT_FREE_AND_EXIT_CHECK )
extern void	abort(int);
extern void	free(void*);
extern void	exit(void*);
#endif  /* INT_ABORT_FREE_AND_EXIT_CHECK */


#if defined( NEWS_OS_RECURSION_CHECK )
#ifdef BOGUS_RECURSION
#include <stdlib.h>
#endif
#endif  /* NEWS_OS_RECURSION_CHECK */


#if defined( SVR4_GETCWD_CHECK )
extern char* getcwd(char *, size_t);
#endif  /* SVR4_GETCWD_CHECK */


#if defined( SVR4_PROFIL_CHECK )
profil(unsigned short *, size_t, int, unsigned int);
#endif  /* SVR4_PROFIL_CHECK */


#if defined( SYSZ_STDLIB_FOR_SUN_CHECK )
extern void *	calloc(size_t);
extern void *	malloc(size_t);
extern void *	realloc(void*,size_t);
extern void *	bsearch(void*,size_t,size_t);

#endif  /* SYSZ_STDLIB_FOR_SUN_CHECK */
