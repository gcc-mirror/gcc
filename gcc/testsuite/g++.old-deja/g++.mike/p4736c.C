// { dg-do run  }
// prms-id: 4736

int did_fail;

class Rep {
public:
  virtual ~Rep() { }
};

class Rep_1 : public Rep {
};

class VBaseMain {
public:
  virtual ~VBaseMain() { }
};

class OtherVBase {
public:
  virtual ~OtherVBase() { }
};

class Rep_2 : public Rep {
};

class DVBase : public VBaseMain, public Rep_2, public OtherVBase {
public:
  virtual ~DVBase() { }
};

class Main : public Rep_1, virtual public DVBase {
public:
  virtual ~Main() { did_fail = 0; }
};

int main() {
  Main* m;
  did_fail = 1;
  delete new Main;
  if (did_fail)
    return 1;
  did_fail = 1;
  delete (Rep*)(Rep_1*)new Main;
  if (did_fail)
    return 2;
  did_fail = 1;
  delete (DVBase*)new Main;
  if (did_fail)
    return 3;
  did_fail = 1;
  delete (VBaseMain*)(DVBase*)new Main;
  if (did_fail)
    return 4;
  did_fail = 1;
  delete (Rep*)(Rep_2*)(DVBase*)new Main;
  if (did_fail)
    return 5;
  did_fail = 1;
  delete (OtherVBase*)(DVBase*)new Main;
  if (did_fail)
    return 6;
}
