// { dg-do assemble  }
// { dg-options "-frtti" }
// GROUPS passed rtti
// dossier file
// Message-Id: <9212021501.AA02484@olympia.miro.com>
// From: rme@miro.com (Richard M. Emberson)
// Subject: bug
// Date: Wed, 2 Dec 92 07:01:30 PST

class Vector {
  int *p;
  int sz;
public:
  Vector(int );
  ~Vector();

  int& operator[](int i);
};
