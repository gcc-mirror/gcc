// { dg-do assemble  }

template <int dim>
class Point {
  public:
    Point (Point<dim> &);
    Point<dim> & operator = (Point<dim> &);
};



template <int dim>
class bar{
  public:
    void foo (Point<dim> p = Point<dim>());
};



template <>
void bar<2>::foo (Point<2> p) {
  const int dim = 2;
  Point<dim> q = p;
}
