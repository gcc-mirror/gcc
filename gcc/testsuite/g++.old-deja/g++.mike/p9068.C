// prms-id: 9068

struct ostream {
  void operator<< (int);	// ERROR - fn ref in err msg
};

class C {
public:
  static int& i ();
  static int& i (int signatureDummy);
};

void foo (ostream& lhs, const C& rhs)
{
  lhs << rhs.i;		// ERROR - no such i for any opr << ()
}

int& C::i () {
  static int _i = 4711;
  return _i;
}
