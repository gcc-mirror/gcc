// prms-id: 11110

class data;

class conatiner {
public:
  virtual void* first    ();
  virtual data* contents (void* i);     // ERROR - candidates
};

class user {
public:
  data* data1 () const;
private:
  conatiner& _c;
};

data* user::data1() const {
  return (_c.contents (_c.first));	// ERROR - 
}                                       // ERROR - control reaches end
