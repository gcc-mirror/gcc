// { dg-do run  }
// prms-id: 10148

int fail = 1;
void ok() { fail = 0; }

class TC {
  int s_;
};

class TIRD {
 public:
  void (*itc)();
  TIRD() { itc = ok; }
};

class TCCB : public TC, public TIRD {
};

class TCRCB : public TCCB {
public:
  virtual void eat ();
};

void TCRCB::eat () {
 void *vp = (TIRD*)this->itc;
 this->itc();
}

int main() {
  TCRCB a;
  a.eat();
  return fail;
}
