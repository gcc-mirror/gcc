

#if defined( BAD_LVAL_CHECK )
#pragma extern_prefix "_FOO"
#define something _FOOsomething
#define mumble _FOOmumble
#endif  /* BAD_LVAL_CHECK */
