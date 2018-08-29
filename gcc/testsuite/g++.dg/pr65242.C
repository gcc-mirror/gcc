/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3" } */

class A {
public:
  int m_fn1();
};
class B {
public:
  enum IOMode { reading };
};
class tn_file_buf_stream : B {
public:
  tn_file_buf_stream(IOMode);
  ~tn_file_buf_stream();
};
class C {
public:
  int &operator[](int);
};
class D {
public:
  bool m_fn2();
};
class F {
public:
  int m_fn3(D &);
};
class G {
public:
  D bdt;
};
class ObjectType {
public:
  int id;
  D weather;
  struct H {
    F terrainaccess;
  };
  H m_fn4();
  struct {
    A images;
  } weatherPicture[];
  ObjectType *m_fn5();
  int m_fn6();
} a;
#pragma pack(1)
class I {};
class J {
  J(I *);
  I translationTableTMISSPart;
  void m_fn8();
  tn_file_buf_stream *MissFile;
  void m_fn9();
  virtual G *m_fn7(int, int);
};
int b, c, d, g;
int e[5];
short f;
void J::m_fn9() {
  int h;
  C k;
  for (; b;) {
    int l = c, n = c & 1;
    for (int m; d;) {
      int o = 0;
      for (int p = 0; p < 2 && !o; p++)
        if (g)
          for (int i; i < a.m_fn6(); i++) {
            ObjectType *q = a.m_fn5();
            for (int r = 0; r < 6; r++)
              if (q->weather.m_fn2())
                for (int j; j < q->weatherPicture[r].images.m_fn1(); j++)
                  if (e[m]) {
                    G *s = m_fn7(l, n);
                    if (q->m_fn4().terrainaccess.m_fn3(s->bdt))
                      g = o = 1;
                  }
          }
      k[h++] = f;
    }
  }
}

void J::m_fn8() try {
  tn_file_buf_stream t(B::reading);
  MissFile = &t;
  m_fn9();
  J u(0);
  u.m_fn8();
}

catch (int) {
}
