// { dg-do compile }

// Origin: Matt Austern <austern@apple.com>

// PR c++/19258: Wrong lookup scope for friend defined in class.

class X {
  template<class T> friend int ff(T*, int y=anX.x) { return y; }
  int f() { return ff(&anX); }

  static X anX;
  int x;
};

X dummy;
