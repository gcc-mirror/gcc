// Build don't link: 
// GROUPS passed temps
// temps file
// Date: Mon, 07 Sep 1992 13:12:28 EDT
// From: richard@ttt.kth.se 
struct foo
{
  char *s;
  foo(char *x) { s=x; }
};

struct cookie
{
  foo * v;
  cookie ( foo * x) { v=x; }
};

cookie cat(&foo("apabepa"));// ERROR - .*
