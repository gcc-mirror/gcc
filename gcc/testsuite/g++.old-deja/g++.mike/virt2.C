struct S0 {
  virtual int is_kind_of_S1 () const { return 0; }
  virtual void dummy () { }
};

struct S1 : virtual public S0 {
  virtual int is_kind_of_S1 () const { return 1; }
  virtual void dummy () { }
};

struct S2 : virtual public S0 {
  virtual void dummy () { }
};

struct S3 : public S2, public S1 {
  virtual void dummy () { }
};

static struct S0 *var = new S3 ();

int main () {
  if (var->is_kind_of_S1() != 1)
    return 1;
  return 0;
}
