
#define DEF(TYPE) \
  void \
  foo_ ## TYPE ## _d (TYPE *a, TYPE *output) \
  { \
    int i; \
    for (i = 0; i < 8 / sizeof (TYPE); i++) \
      output[i] = *a; \
  } \
  void foo_ ## TYPE ## _q (TYPE *a, TYPE *output) \
  { \
    int i; \
    for (i = 0; i < 32 / sizeof (TYPE); i++) \
      output[i] = *a; \
  }
