#define FOR_EACH_SCALAR(F)	\
  F(sc, signed char)		\
  F(uc, unsigned char)		\
  F(ss, short)			\
  F(us, unsigned short)		\
  F(si, int)			\
  F(ui, unsigned int)		\
  F(sl, long)			\
  F(ul, unsigned long)		\
  F(sll, long long)		\
  F(ull, unsigned long long)	\
  F(f, float)			\
  F(d, double)			\
  F(ld, long double)		\
  F(ptr, void *)

#define EXTERN(SUFFIX, TYPE) extern TYPE x##SUFFIX;
#define STATIC(SUFFIX, TYPE) static TYPE s##SUFFIX;
#define COMMON(SUFFIX, TYPE) TYPE c##SUFFIX;

#define GETADDR(SUFFIX, TYPE)			\
  TYPE *get##SUFFIX (int which)			\
  {						\
    return (which == 0 ? &c##SUFFIX		\
	    : which == 1 ? &s##SUFFIX		\
	    : &x##SUFFIX);			\
  }

#define COPY(SUFFIX, TYPE) c##SUFFIX = s##SUFFIX; s##SUFFIX = x##SUFFIX;

FOR_EACH_SCALAR (EXTERN)
FOR_EACH_SCALAR (STATIC)
FOR_EACH_SCALAR (COMMON)

FOR_EACH_SCALAR (GETADDR)

void
copy (void)
{
  FOR_EACH_SCALAR (COPY);
}

extern void foo (int);

void
sibcall1 (void)
{
  foo (1);
}

void
sibcall2 (void)
{
  foo (csi + ssi + xsi);
}

static void
sibcall3 (void)
{
  foo (1);
  foo (2);
  foo (3);
}

extern void bar (void (*) (void));

int
nested (int x)
{
  void sub (void) { foo (x); }
  bar (sub);
  bar (sibcall3);
  return 1;
}
