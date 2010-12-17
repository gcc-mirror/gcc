// { dg-do assemble  }
// GROUPS passed temps
// temps file
// Date: Mon, 07 Sep 1992 13:12:28 EDT
// From: richard@ttt.kth.se 
// { dg-options "-fpermissive" }
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

cookie cat(&foo("apabepa"));// { dg-warning "deprecated conversion" "dep" }
// { dg-warning "taking address of temporary" "add" { target *-*-* } 19 }
