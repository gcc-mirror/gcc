struct bug {
  const char *name;
  unsigned long type;
};

struct bug s = { 0, (unsigned long) &s | 1 };
