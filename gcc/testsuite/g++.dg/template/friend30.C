// PR c++/14930

template<typename T> class Point;

template<> class Point<double> {
  friend class Plane;
  double v;
};

struct Plane {
  double get(const Point<double>& p);
};

double Plane::get(const Point<double> &p) { return p.v; }

