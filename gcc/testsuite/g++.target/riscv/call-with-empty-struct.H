#define MAKE_STRUCT_PASSING_TEST(type,val)                              \
  static struct struct_ ## type ## _t                                   \
  {                                                                     \
    struct { } e;                                                       \
    struct { type f; } s;                                               \
  } global_struct_ ## type = { {}, { val } };                           \
                                                                        \
  static bool                                                           \
  check_struct_ ## type (struct_ ## type ## _t obj)                     \
  {                                                                     \
    return (obj.s.f == global_struct_ ## type .s.f);                    \
  }                                                                     \
                                                                        \
  int                                                                   \
  main ()                                                               \
  {                                                                     \
    bool result = check_struct_ ## type ( global_struct_ ## type );     \
    return result ? 0 : 1;                                              \
  }
