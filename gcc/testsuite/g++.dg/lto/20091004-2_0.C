// { dg-lto-do link }
// { dg-lto-options {{-fPIC -O -flto}} }

typedef double Real;
class Vector {
    int dimen;
    Real* val;
public:
    Vector& operator=(const Vector& vec);
    Vector(int p_dimen, Real *p_val)
	: dimen(p_dimen), val(p_val)    { }
    int dim() const;
};
class DVector : public Vector {
public:
    void reDim(int newdim);
    explicit DVector(const Vector& old);
    DVector& operator=(const Vector& vec)    {
	reDim(vec.dim());
	Vector::operator=(vec);
    }
};
class SLUFactor  {
    DVector vec;
    void solveRight (Vector& x, const Vector& b);
};
void SLUFactor::solveRight (Vector& x, const Vector& b) {
    vec = b;
}
