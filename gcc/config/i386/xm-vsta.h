/* Use semicolons to separate elements of a path.  */
#define PATH_SEPARATOR ';'

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("unix");		\
    }						\
  while (0)

#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"
