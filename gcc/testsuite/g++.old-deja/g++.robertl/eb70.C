// conversion ops should be treated as coming from the most derived class
// for overload resolution.  See [over.match.funcs].
// Build don't link:
// excess errors test - XFAIL *-*-*

class X {
public:
  inline operator bool() const { return true; }
};

class Y : public X {
private:
  inline operator void*() const { return 0; }
};


int f(Y const& y) {
  return bool(y);
}
