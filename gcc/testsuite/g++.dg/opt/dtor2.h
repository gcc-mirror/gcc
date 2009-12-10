struct A
{
  A ();
  ~A ();
};

struct B
{
  A b;
  virtual void mb ();
  B (int);
  virtual ~B ();
};

struct C : public B
{
  virtual void mc ();
  C (int);
  ~C ();
};

inline C::~C () {}

struct D : public C
{
  A d;
  D (int);
  ~D ();
};
