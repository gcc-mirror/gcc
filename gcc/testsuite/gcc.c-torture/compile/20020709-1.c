extern double atof (__const char *__nptr) __attribute__ ((__pure__));

void bar (char *s)
{
  union {double val; unsigned int a, b;} u;
  u.val = atof (s);
}
