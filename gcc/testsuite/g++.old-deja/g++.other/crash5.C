// Build don't link:

class TecMesh {};

extern TecMesh& m;

struct X { 
  X(TecMesh&);
};

struct D  {
  D();
  TecMesh& Mesh;
};


D::D ()
  : Mesh(m)
{
  X x(D::Mesh);
}
