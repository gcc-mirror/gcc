// Build don't link:

template<class CoordinateSystem, class MeshTag>
struct Mesh { };

struct RectGrid { };

struct RectMesh { };

struct Cartesian { };

template<class CS>
struct Mesh<CS, RectGrid> { };

template<class CS>
struct Mesh<CS, RectMesh> : public Mesh<CS, RectGrid> { };

template<class CS>
void foo(const Mesh<CS, RectGrid> &)
{
}

int main()
{
  Mesh<Cartesian, RectMesh> m;
  foo(m);
}
