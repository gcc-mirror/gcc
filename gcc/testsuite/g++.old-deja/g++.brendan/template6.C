// { dg-do assemble  }
// GROUPS passed templates
struct B {
};

struct X : B {
  ~X ();
};

struct Request {
  X s;
};

template <class ET> class TC {
  ET data;
};

struct TMem {

  ~TMem() {}

  TC<Request> *req;
};

struct FIO {

  void init ();

  FIO () { init(); }
};
