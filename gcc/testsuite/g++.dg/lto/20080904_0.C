// { dg-lto-do run }

/* This test will fail to link if the vtable for Derived is not emitted.  */

class Base {
public:
  Base(char *buf, unsigned len)
    : _buf(buf),
      _len(len)
  {}

  virtual int length () { return _len; }

private:
  char * _buf;
  unsigned _len;
};

class Derived : public Base {
public:
  Derived(char *buf, unsigned len)
    : Base(buf, len),
      _ctr(len)
  {}

  virtual int length () { return _ctr; }

private:
  unsigned _ctr;
};

int main ()
{
  Derived *d = new Derived (new char[256], 256);

  return 0;
}
