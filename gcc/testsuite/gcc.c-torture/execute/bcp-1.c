int global;

/* These must fail.  */
int bad0(void) { return __builtin_constant_p(global); }
int bad1(void) { return __builtin_constant_p(global++); }
inline int bad2(int x) { return __builtin_constant_p(x++); }
inline int bad3(int x) { return __builtin_constant_p(x); }
inline int bad4(const char *x) { return __builtin_constant_p(x); }
int bad5(void) { return bad2(1); }
inline int bad6(int x) { return __builtin_constant_p(x+1); }
int bad7(void) { return __builtin_constant_p(abort()); }
int bad8(void) { char buf[10]; return __builtin_constant_p(buf); }
int bad9(const char *x) { return __builtin_constant_p(x[123456]); }

/* These must pass, or we've broken gcc2 functionality.  */
int good0(void) { return __builtin_constant_p(1); }
int good1(void) { return __builtin_constant_p("hi"); }
int good2(void) { return __builtin_constant_p((1234 + 45) & ~7); }

/* These are extensions to gcc2.  Failure indicates an optimization
   regression.  */
int opt0(void) { return bad3(1); }
int opt1(void) { return bad4("hi"); }
int opt2(void) { return bad6(1); }
int opt3(void) { return __builtin_constant_p(&global); }
int opt4(void) { return __builtin_constant_p("hi"[0]); }

/* Call through tables so -finline-functions can't screw with us.  */
int (*bad_t0[])(void) = {
	bad0, bad1, bad5, bad7, bad8
};

int (*bad_t1[])(int x) = {
	bad2, bad3, bad6
};

int (*bad_t2[])(const char *x) = {
	bad4, bad9
};

int (*good_t0[])(void) = {
	good0, good1, good2
};

int (*opt_t0[])(void) = {
	opt0, opt1, opt2, opt3, opt4
};

#define N(arr) (sizeof(arr)/sizeof(*arr))

int main()
{
  int i;

  for (i = 0; i < N(bad_t0); ++i)
    if ((*bad_t0[i])())
      abort();

  for (i = 0; i < N(bad_t1); ++i)
    if ((*bad_t1[i])(1))
      abort();

  for (i = 0; i < N(bad_t2); ++i)
    if ((*bad_t2[i])("hi"))
      abort();

  for (i = 0; i < N(good_t0); ++i)
    if (! (*good_t0[i])())
      abort();

#ifdef __OPTIMIZE__
  for (i = 0; i < N(opt_t0); ++i)
    if (! (*opt_t0[i])())
      abort();
#endif

  exit(0);
}
