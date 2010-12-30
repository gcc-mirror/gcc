// { dg-do assemble  }
// prms-id: 9068

struct ostream {
  void operator<< (int);	// { dg-message "operator|no known conversion" } fn ref in err msg
};

class C {
public:
  static int& i ();
  static int& i (int signatureDummy);
};

void foo (ostream& lhs, const C& rhs)
{
  lhs << rhs.i;		// { dg-error "match" } no such i for any opr << ()
  // { dg-message "candidate" "candidate note" { target *-*-* } 16 }
}

int& C::i () {
  static int _i = 4711;
  return _i;
}
