// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed conversions
// cvt file
// Message-Id: <m0mVSRj-0000cEC@mobius.veritas.com>
// From: joe@veritas.com (Joe Fasano)
// Subject: gcc-2.2.2 bug report
// Date: Thu, 17 Sep 92 13:27 PDT

typedef int (*widget) ();

class window {
public:
  int Isopen ();
};

widget fp = (widget) &window::Isopen;	// { dg-error "" } // ERROR - 
