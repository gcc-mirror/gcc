// PR c++/38232

class base {
 public:
  base();
  virtual ~base();

 private:
  int& int_ref;  // initialized by base ctor, not visible here
};

class derived : public base {
};

base *make_derived() {
  return new derived();
}
