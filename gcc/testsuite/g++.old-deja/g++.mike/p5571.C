// prms-id: 5571

int err = 0;
void *vp = 0;

class ParentOne {
public:
  ParentOne() {};
#ifdef MAKE_WORK
  virtual ~ParentOne() {};
#endif
private:
  char SomeData[101];
};

class ParentTwo {
public:
  ParentTwo() {};
  virtual ~ParentTwo() {};
private:
  int MoreData[12];
  virtual int foo() { return 0; }
};

struct Child : public ParentOne, public ParentTwo {
    int ChildsToy;
    virtual void PrintThis() = 0;
};

struct Student : public Child {
  int StudentsBook;
  void PrintThis() {
    if (vp == 0)
      vp = (void *)this;
    else
      {
	if (vp != (void *)this)
	  ++err;
      }
  }
  void LocalPrintThis() {
    if (vp == 0)
      vp = (void *)this;
    else
      {
	if (vp != (void *)this)
	  ++err;
      }
    PrintThis();
  }
  void ForcedPrintThis() {
    if (vp == 0)
      vp = (void *)this;
    else
      {
	if (vp != (void *)this)
	  ++err;
      }
    Student::PrintThis();
  }
};

int main() {
  Student  o;
  o.LocalPrintThis();
  o.ForcedPrintThis();
  Child* pX = &o;
  pX->PrintThis();
  return err;
}
