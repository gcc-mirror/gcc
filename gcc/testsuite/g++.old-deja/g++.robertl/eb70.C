// Build don't link:  

class X {
public:
  inline operator bool() const { return true; }
};

class Y : public X {
private:
  inline operator void*() const { return 0; }
};


void f(Y const& y) {
  if( bool(y) ) {
  }
}
