// Testcase for wrongful generation of operator =.
// Build don't link:

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

