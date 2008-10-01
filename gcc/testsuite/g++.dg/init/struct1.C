struct bug {
  const char *name;
  __SIZE_TYPE__ type;
};

struct bug s = { 0, (__SIZE_TYPE__) &s | 1 };
