// { dg-do compile }
class A {
  virtual unsigned long write(const char *, unsigned long);
};
char *a;
int b;
bool c;
char e[16];
class B {
public:
  void push_range(const char *);
};
class C : A {
  B m_string;

public:
  unsigned long write(const char *p1, unsigned long p2) {
    m_string.push_range(p1 + p2);
    return 0;
  }
};
char *write_signed_decimal_backward(bool) {
  char *d = 0;
  if (b) {
    if (c)
      --a;
    d = a;
  }
  return d;
}

template <typename TextOutputStreamType>
void ostream_write(TextOutputStreamType &p1) {
  char *f = write_signed_decimal_backward(false);
  p1.write(f, e - f);
}

void operator<<(C p1, int) { ostream_write(p1); }
