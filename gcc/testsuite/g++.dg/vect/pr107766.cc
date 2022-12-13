// { dg-do compile }
// { dg-additional-options "-ffp-contract=off" }

typedef double btScalar;
struct btVector3 {
  operator btScalar *() const;
};
double m_vec[2];
struct btShapeMatrix {
  double &operator[](int i) { return m_vec[i]; }
};
btScalar shape_function___1pxt1pz, shape_function__fac;
struct btMiniSDF {
  void shape_function_(btVector3 const &) const;
};
void btMiniSDF::shape_function_(btVector3 const &xi) const {
  btShapeMatrix res;
  btScalar _1m3y = 1.0 - 3.0 * xi[1], _1p3y = 1.0 + 3.0 * xi[1],
           fact1m3y = shape_function__fac * _1m3y,
           fact1p3y = shape_function__fac * _1p3y;
  res[22] = fact1m3y * shape_function___1pxt1pz;
  res[23] = fact1p3y * shape_function___1pxt1pz;
}
