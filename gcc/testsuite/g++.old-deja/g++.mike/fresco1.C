// { dg-do assemble  }

// This is a Fresco found bug.

class WhoCares {};

typedef float Coord;

class BugDemo : public WhoCares {
public:
  typedef Coord Matrix[4][4];
  virtual void vf1(BugDemo::Matrix m) = 0;
};
