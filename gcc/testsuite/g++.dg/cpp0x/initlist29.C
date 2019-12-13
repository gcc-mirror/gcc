// PR c++/42331
// { dg-do compile { target c++11 } }

class Mesh
{
public:
  Mesh(const char*)
  { typele={0}; }		// { dg-error "11:assigning to an array from an initializer list" }

private:
  int typele[7][2];
};

Mesh m(0);
