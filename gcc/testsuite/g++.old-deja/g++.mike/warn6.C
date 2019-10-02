// { dg-do assemble  }
// { dg-options "-Woverloaded-virtual" }

struct B4 {
  virtual void bothfardiff(float);	// { dg-warning "" } was hidden
};

struct B3 : public B4 {
};

struct B2 {
};

struct B {
  virtual void baseonly(int);

  virtual void bothsame(int);

  virtual void bothdiff(float);		// { dg-warning "" } was hidden

  virtual void both2same(int);
  virtual void both2same(float);

  virtual void both12diff(int);
  virtual void both12diff(float);	// { dg-warning "" } was hidden
};

struct D : public B, public B2, public B3 {
  virtual void derivedonly(int);

  virtual void bothsame(int);

  virtual void bothdiff(int);		// { dg-message "" } 

  virtual void both2same(int);
  virtual void both2same(float);

  virtual void both12diff(int);		// { dg-message "" } 

  virtual void bothfardiff(int);	// { dg-message "" } 
};

