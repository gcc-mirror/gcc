// Build don't link:
// prms-id: 4693

class a {
public:
  virtual ~a();
};

class b {
public:
  virtual void set_var() = 0;
};

class c : public b, public a { };

class d : public c {
public:
  void set_var() { }
};

int main() {
  d * test;
  test = new d;
}
