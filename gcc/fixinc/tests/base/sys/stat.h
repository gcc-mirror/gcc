

#if defined( M88K_BAD_S_IF_CHECK )
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG) /* is regular? */
#endif  /* M88K_BAD_S_IF_CHECK */


#if defined( RS6000_FCHMOD_CHECK )
extern int fchmod(int, mode_t);
#endif  /* RS6000_FCHMOD_CHECK */


#if defined( SCO_STATIC_FUNC_CHECK )
#ifdef __STDC__
#if __cplusplus
extern "C" {
#endif /* __cplusplus */
static int	stat(const char *__f, struct stat *__p) {
return __stat32(__f, __p);
}
#if __cplusplus
 }
#endif /* __cplusplus */

#  else /* !__STDC__ */
#if __cplusplus
extern "C" {
#endif /* __cplusplus */
static int	stat(__f, __p)
char *__f;
struct stat *__p;
{
return __stat32(__f, __p);
}
#if __cplusplus
 }
#endif /* __cplusplus */
#endif
#endif  /* SCO_STATIC_FUNC_CHECK */
