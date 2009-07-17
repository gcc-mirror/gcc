/* { dg-do compile } */

namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     { };
    template<typename _Tp>     class allocator: public new_allocator<_Tp> { };
    template<typename _Tp, typename _Alloc>     struct _Vector_base     { };
    template<typename _Tp, typename _Alloc = std::allocator<_Tp> >
    class vector : protected _Vector_base<_Tp, _Alloc>     { };
};
template<int Dim> class Vector { };
enum CenteringType { VertexType,  EdgeType,  FaceType,  CellType };
enum ContinuityType { XDim = 1,  YDim = XDim << 1,  ZDim = YDim << 1 };
template <int Dim> class Centering {
public:
    typedef Vector<Dim> Position;
    typedef std::vector<Position> Positions;
    Centering(const Positions &positions);
    Positions positions_m;
};
template <int Dim> class CanonicalCentering {
    CanonicalCentering();
    template <class T> static T combine(const T &op1, const T &op2);
    static Centering<Dim>*** centering_table_m;
};
template <int Dim> CanonicalCentering<Dim>::CanonicalCentering()
{
  typename Centering<Dim>::Positions positions[Dim][2];
  enum { x = 0, y, z };
  int cont = 0;
  if (Dim > 1)
    {
      centering_table_m[EdgeType][cont][YDim] =  Centering<Dim>(positions[y][cont]);
      centering_table_m[EdgeType][cont][XDim|YDim] =  Centering<Dim>(combine(positions[x][cont], positions[y][cont]));
    }
  if (Dim > 2)
    {
      centering_table_m[EdgeType][cont][ZDim] =  Centering<Dim>(positions[z][cont]);
      centering_table_m[EdgeType][cont][XDim|ZDim] =  Centering<Dim>(combine(positions[x][cont], positions[z][cont]));
    }
}
template class CanonicalCentering<2>;
