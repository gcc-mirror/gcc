

#if defined( SUN_MALLOC_CHECK )
typedef void *	malloc_t;
void	free();
void*	malloc();
void*	calloc();
void*	realloc();
#endif  /* SUN_MALLOC_CHECK */
