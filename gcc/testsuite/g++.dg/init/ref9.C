// { dg-do run }

struct ex;
struct basic {
  int refcount;
  ex eval() const;
  basic() : refcount(0) {}
};

struct ex {
  basic *bp;
  ex() : bp(0) { }
  ex(const basic &);
  virtual ~ex();
  void construct_from_basic(const basic &);
};

ex basic::eval() const {
  throw 1;
}

inline ex::ex(const basic &b) { construct_from_basic (b); }
inline ex::~ex() { if (--bp->refcount == 0) delete bp; }
void ex::construct_from_basic(const basic &b) {
  const ex & tmpex = b.eval();
  bp = tmpex.bp;
  bp->refcount++;
}

ex pow() { return basic(); }

int main()
{
  try { pow (); } catch (int) {}
  return 0;
}
