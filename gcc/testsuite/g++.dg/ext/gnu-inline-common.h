#ifndef gnu
# define gnu_inline __attribute__((gnu_inline)) inline
#endif

#define declspec(spec, name) spec int name (void)
#ifdef IN_CLASS
# define decl(spec, name)
#else
# define decl(spec, name) defpfx declspec(spec, name);
#endif
#define def(spec, name, ret) defpfx declspec(spec, name) { return ret; }
#define gnuindef(name, ret) def(gnu_inline, name, ret)

#ifndef pfx
# ifdef IN_CLASS
#  define pfx(x) IN_CLASS::x
# else
#  define pfx(x) x
# endif
#endif

#ifndef defpfx
# define defpfx
#endif
