// PR debug/61033
// { dg-do compile }
// { dg-options "-g -O2" }

# 0 "" 3
struct A {
  A(int) : ch() {}
  unicode() {}
  int ch;
};
class QChar {
public:
  QChar(A ch) : ucs(ch.unicode()) {}
  int ucs;
};
class B {
public:
  int isShared_count;
  isShared() { return isShared_count; }
};
struct C {
  unsigned short *data() {
    void *__trans_tmp_1;
    __trans_tmp_1 = reinterpret_cast<char *>(0) + offset;
    return static_cast<unsigned short *>(__trans_tmp_1);
  }
  B ref;
  int size;
  int alloc;
  int offset;
};
class D {
public:
  D(char *) : m_data() {} * m_data;
};
class F {
public:
  F(int, QChar);
  F(D);
  operator+=(QChar) {
    if (d->ref.isShared() || d->alloc)
      reallocData();
    d->data()[d->size++] = 0;
    d->data()[0] = '\0';
  }
  C *d;
  reallocData();
};
struct G {
  struct H {
    int begin;
  };
  H d;
  size() { return d.begin; }
};
class I {
  G p;
public:
  ~I();
  length() { return p.size(); }
};
class J;
class K {
public:
  J toNodeListProperty() const;
};
class L {
  F toQml(const K &property) const;
  toQml() const;
};
class J {
public:
  I toModelNodeList();
};
F::F(D) {}
F L::toQml(const K &property) const {
  I nodes = property.toNodeListProperty().toModelNodeList();
  F result("");
  F(0, A(' '));
  for (int i = 0; i < nodes.length(); ++i) {
    if (i)
      result += A(',');
    result += A('\n');
    toQml();
  }
}
