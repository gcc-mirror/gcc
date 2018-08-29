// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

typedef __SIZE_TYPE__ size_t;

class c1;

class c2 {
  public: c2() { };
  void *operator new(size_t size, const c1 & crc1);
};

class c3 {
  public: c3() { _Obj = 0; }
  ~c3() { if (_Obj) delete _Obj; }
  void set(c2 *pObj);
  protected: c2 *_Obj;
};

void c3::set(c2 *pObj) { _Obj = pObj; };

template<class TYPE> class tc1 : public c2 {
  public: tc1(int n=0){};
  int get() const;
  TYPE& operator[] (int id);
  TYPE * _data;
  int _size;
};

template<class TYPE> TYPE & tc1<TYPE>::operator[] (int id) {
  return _data[id];
}
 
template<class TYPE> int tc1<TYPE>::get() const {
  return _size;
}

class c4 {
  public: c4();
};

class c5 : public c2 {
  protected: c2 * _own;
  public: c5(c2 *o) : _own(o) { }
  c5(const c4 & box);
  int add(const c4 & ext);
};

class c6 {
  public: int get() const { return 0; };
};

class c7 {
  friend class c8;
  int find(c6 * loop) const;
};

class c8 {
  const c1 & _rc1;
  int tria(c7 * face, c5 * vtree0 = 0);
};

int c8::tria(c7 * face, c5 * vtree0) {
  c6 *sLData[64];
  tc1<c6*> loops(64);
  while (loops.get() > 1) { 
    c6 *iloop = 0; 
    for (int j=1; j<loops.get(); j++) { 
      if (loops[j]->get() < 32) { 
        iloop = loops[j];
      }
    }
    face->find(iloop);
  }
  c4 box;
  c3 ctree;
  c5 *vtree = vtree0;
  if (!vtree) { 
    vtree = new (_rc1) c5(box); 
    ctree.set(vtree); 
    for (int j=0; j<1; j++) { 
      c4 sVBBox; 
      vtree->add(sVBBox);
    }
  }
}
