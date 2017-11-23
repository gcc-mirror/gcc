
#define PRE_STORE(T)			\
  T *					\
  T ## _pre_store (T *p, T v)		\
  {					\
    *++p = v;				\
    return p;				\
  }					\

#define POST_STORE(T)			\
  T *					\
  T ## _post_store (T *p, T v)		\
  {					\
    *p++ = v;				\
    return p;				\
  }

#define POST_STORE_VEC(T, VT, OP)	\
  T *					\
  VT ## _post_store (T * p, VT v)	\
  {					\
    OP (p, v);				\
    p += sizeof (VT) / sizeof (T);	\
    return p;				\
  }

#define PRE_LOAD(T)			\
  void					\
  T ## _pre_load (T *p)			\
  {					\
    extern void f ## T (T*,T);		\
    T x = *++p;				\
    f ## T (p, x);			\
  }

#define POST_LOAD(T)			\
  void					\
  T ## _post_load (T *p)		\
  {					\
    extern void f ## T (T*,T);		\
    T x = *p++;				\
    f ## T (p, x);			\
  }

#define POST_LOAD_VEC(T, VT, OP)	\
  void					\
  VT ## _post_load (T * p)		\
  {					\
    extern void f ## T (T*,T);		\
    VT x = OP (p, v);			\
    p += sizeof (VT) / sizeof (T);	\
    f ## T (p, x);			\
  }
