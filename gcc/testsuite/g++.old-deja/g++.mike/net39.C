void *vp;

class silly {
public:
  virtual int b() { return 1; }
};
class solly : silly {
public:
  virtual int b() { return 2; }
};
class thing {
public:
  virtual int a() { return 3; }
};
class thong : public solly, public thing {
public:
  virtual int a() {
    if (this != vp) return 4;
    else return 0;
  }
};

typedef int(thing::*ping)();
ping qq = &thing::a;

int main() {
  thong b;
  vp = &b;
  return (b.*qq)();
}
