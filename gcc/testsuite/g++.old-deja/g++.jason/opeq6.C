// { dg-do assemble  }
// Testcase for wrongful generation of operator =.

class ivResource {
public:
  ivResource ();
private:
  ivResource & operator =(const ivResource &);
};

class ivButtonState : virtual public ivResource {
public:
  void operator=(ivButtonState &);
};

class ivPrintBS : public ivButtonState {
};

void f ()
{
  ivPrintBS a, b;
  a = b;
}

