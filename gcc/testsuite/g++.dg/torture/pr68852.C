/* { dg-do compile } */

struct A {
    double x, y, z, w;
    A() {}
    A(double, double p2, double p3, double) : y(p2), z(p3) {}
    void m_fn1();
};

struct B {
    double x, y;
};
struct D : A {
    D() {}
    D(double p1, double p2, double p3, double p4) : A(p1, p2, p3, p4) {}
};

class C {
public:
    float _11, _12, _13, _14;
    float _21, _22, _23, _24;
    float _31, _32, _33, _34;
    float _41, _42, _43, _44;
    D m_fn2(B p1) {
	double z(p1.x + _43);
	return *this * D(p1.x, p1.y, z, 1);
    }
    int ProjectRectBounds_next;
    B __trans_tmp_3;
    int m_fn3(int) {
	B a, b;
	D c[1];
	b = __trans_tmp_3;
	c[2] = m_fn2(b);
	c[3] = m_fn2(a);
	c[ProjectRectBounds_next].m_fn1();
	return 0;
    }
    D operator*(D p1) {
	D d;
	d.x = p1.x * _11 + p1.y * _21 + p1.z * _31 + _41;
	d.y = p1.x * _12 + p1.y * _22 + p1.z * _32 + _42;
	d.z = p1.x * _13 + p1.y * _23 + p1.z * _33 + _43;
	d.w = p1.x * _14 + p1.y * _24 + p1.z * _34 + _44;
	return d;
    }
};

void fn1() {
    C e;
    int f = e.m_fn3(f);
}
