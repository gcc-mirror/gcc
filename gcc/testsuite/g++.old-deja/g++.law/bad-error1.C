// { dg-do assemble  }
// GROUPS passed bad-errors
// bad-error file
// Message-Id: <9301081103.AA29469@jclark.com>
// From: jjc@jclark.com (James Clark)
// Subject: initializer for static class member array
// Date: Fri, 8 Jan 93 11:03:05 GMT

struct A {
  static int v[];
};

int A::v[1] = { 1 };

