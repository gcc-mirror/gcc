// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <9308122113.AA14119@tnt.acsys.com>
// From: mclaugh@tnt.acsys.com (Mark A. McLaughlin)
// Subject: g++ bug
// Date: Thu, 12 Aug 93 15:13:23 MDT


class C { };

void f(C) { }

void g(const C & c) { f(c); }

void h(const C & c) { f(C(c)); }
