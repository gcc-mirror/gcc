// { dg-do run  }
// GROUPS passed inlining
// inline file
// Message-Id: <9306020823.AA14027@joker>
// From: stefan@mpi-sb.mpg.de
// Subject: gcc-2.4.2  template function bug  (1)
// Date: Wed, 2 Jun 93 10:23:14 +0200

extern "C" int printf (const char *, ...);

template <class T> inline T func(const T& x) { return x; }

inline int func(const int& x) { return x; }


int main()
{ int x;
  func(x);
  printf ("PASS\n");
 }

