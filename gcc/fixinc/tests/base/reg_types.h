

#if defined( OSF_NAMESPACE_A_CHECK )
typedef struct {
  int stuff, mo_suff;
} regex_t;
extern __regex_t    re;
extern __regoff_t   ro;
extern __regmatch_t rm;

#endif  /* OSF_NAMESPACE_A_CHECK */
