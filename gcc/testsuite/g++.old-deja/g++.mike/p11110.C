// { dg-do assemble  }
// prms-id: 11110

class data;

class conatiner {
public:
  virtual void* first    ();
  virtual data* contents (void* i);
};

class user {
public:
  data* data1 () const;
private:
  conatiner& _c;
};

data* user::data1() const {
  return (_c.contents (_c.first)); // { dg-error "invalid use of non-static member function" }
}
