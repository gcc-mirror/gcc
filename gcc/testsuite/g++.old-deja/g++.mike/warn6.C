// Build don't link:
// Special g++ Options: -Woverloaded-virtual

struct B4 {
  virtual void bothfardiff(float);	// WARNING - was hidden
};

struct B3 : public B4 {
};

struct B2 {
};

struct B {
  virtual void baseonly(int);

  virtual void bothsame(int);

  virtual void bothdiff(float);		// WARNING - was hidden

  virtual void both2same(int);
  virtual void both2same(float);

  virtual void both12diff(int);
  virtual void both12diff(float);	// WARNING - was hidden
};

struct D : public B, public B2, public B3 {
  virtual void derivedonly(int);

  virtual void bothsame(int);

  virtual void bothdiff(int);		// WARNING - 

  virtual void both2same(int);
  virtual void both2same(float);

  virtual void both12diff(int);		// WARNING - 

  virtual void bothfardiff(int);	// WARNING - 
};

