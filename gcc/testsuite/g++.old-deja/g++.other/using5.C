// Build don't link:

// Based on bug report by Klaus-Georg Adams
// <Klaus-Georg.Adams@chemie.uni-karlsruhe.de>

// crash test - XFAIL *-*-*

struct bar {
  typedef bar t;
};

struct foo : bar {
  using bar::t;
  void baz(t pos);
};
