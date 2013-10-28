// PR c++/42331
// { dg-options "-std=c++11" }

class Mesh
{
public:
  Mesh(const char*)
  { typele={0}; }		// { dg-error "" }

private:
  int typele[7][2];
};

Mesh m(0);
