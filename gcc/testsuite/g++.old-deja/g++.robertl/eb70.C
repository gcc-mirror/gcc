// { dg-do assemble  }
// conversion ops should be treated as coming from the most derived class
// for overload resolution.  See [over.match.funcs].

class X {
public:
  operator bool() const;
};

class Y : public X {
private:
  operator void*() const;
};

int f(Y const& y) {
  return bool(y);
}
