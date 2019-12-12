# define __glibc_macro_warning1(message) _Pragma (#message)
# define __glibc_macro_warning(message) \
  __glibc_macro_warning1 (GCC warning message)
#define __SYSMACROS_DM1(...) __glibc_macro_warning (#__VA_ARGS__)

#define __SYSMACROS_DM(symbol) __SYSMACROS_DM1 \
 (Did not Work)

# define major(dev) __SYSMACROS_DM (major) (dev+0)
