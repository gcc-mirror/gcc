// Build don't link:

// Based on bug report by Klaus-Georg Adams
// <Klaus-Georg.Adams@chemie.uni-karlsruhe.de>

struct bar {
  typedef bar t;
};

struct foo : bar {
  using bar::t;
  t field;
  t meth();
  void baz(t arg);
};
