// { dg-do assemble  }
// g++ 1.36.1 bug 900210_10

// g++ allows operator[] to be declared as a static member function.
// This is illegal.

// Cfront 2.0 passes this test.

// keywords: operator[], static function members

struct struct0 {
  static int operator[] ();		/* { dg-error "" "" { target c++20_down } } */
};

int main () { return 0; }
