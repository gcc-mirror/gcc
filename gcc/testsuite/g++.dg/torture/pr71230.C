// { dg-do compile }
// { dg-additional-options "-ffast-math" }

template <int rank, int dim> class Tensor;
template <int dim> class Point {
public:
    Point (const double x, const double y, const double z);
    double operator () (const unsigned int index) const;
};
template <int celldim, int dim> class TriaObjectAccessor  {
    Point<dim> & vertex (const unsigned int i) const;
    Point<dim> barycenter (double, double, double, double, double) const;
};
template <> Point<3> TriaObjectAccessor<3, 3>::barycenter (double s6, double s7, double s1, double s2, double s3) const
{
    const double x[8] = {
	vertex(0)(0),    vertex(1)(0),    vertex(2)(0),    vertex(3)(0),    vertex(4)(0),    vertex(5)(0),    vertex(6)(0),    vertex(7)(0) };
    const double y[8] = {
	vertex(0)(1),    vertex(1)(1),    vertex(2)(1),    vertex(3)(1),    vertex(4)(1),    vertex(5)(1),    vertex(6)(1),    vertex(7)(1) };
    const double z[8] = {
	vertex(0)(2),    vertex(1)(2),    vertex(2)(2),    vertex(3)(2),    vertex(4)(2),    vertex(5)(2),    vertex(6)(2),    vertex(7)(2) };
    double s4, s5, s8;
    const double unknown0 = s1*s2;
    const double unknown1 = s1*s2;
    s8 = -z[2]*x[1]*y[2]*z[5]+z[2]*y[1]*x[2]*z[5]-z[2]*z[1]*x[2]*y[5]+z[2]*z        [1]*x[5]*y[2]+2.0*y[5]*x[7]*z[4]*z[4]-y[1]*x[2]*z[0]*z[0]+x[0]*y[3]*z[7]*z[7]        -2.0*z[5]*z[5]*x[4]*y[1]+2.0*z[5]*z[5]*x[1]*y[4]+z[5]*z[5]*x[0]*y[4]-2.0*z[2]*z        [2]*x[1]*y[3]+2.0*z[2]*z[2]*x[3]*y[1]-x[0]*y[4]*z[7]*z[7]-y[0]*x[3]*z[7]*z[7]+x        [1]*y[0]*z[5]*z[5];
    s5 = s8+z[2]*x[6]*y[2]*z[5]-z[2]*x[5]*y[2]*z[6]-z[2]*x[2]*y[3]*z[7]-x[2]*        y[3]*z[7]*z[7]+2.0*z[2]*x[2]*y[3]*z[1]-z[2]*y[2]*x[3]*z[0]+z[2]*y[2]*x[0]*z[3]-        z[2]*x[2]*y[0]*z[3]-z[7]*y[2]*x[7]*z[3]+z[7]*z[2]*x[7]*y[3]+z[7]*x[2]*y[7]*z[3]        +z[6]*y[1]*x[2]*z[5]-z[6]*x[1]*y[2]*z[5]+z[5]*x[1]*y[5]*z[2]+s6+s7;
    s4 = 1/s5;
    s2 = s3*s4;
    const double unknown2 = s1*s2;
    return Point<3> (unknown0, unknown1, unknown2);
}
