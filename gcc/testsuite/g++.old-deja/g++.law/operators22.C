// { dg-do assemble  }
// GROUPS passed operators
template <class T>
class t {
public:
  t() {}
};

class m {
  t<int> c;
public:
  m() : c() {}
};

m *p() {return new m;}
