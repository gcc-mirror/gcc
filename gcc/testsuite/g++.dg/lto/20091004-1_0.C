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
Vector& Vector::operator=(const Vector& vec)
{
  dimen = vec.dimen;
  val = vec.val;
}
int Vector::dim() const { return dimen; }
DVector::DVector(const Vector& old) : Vector(0, 0)
{
  *this = old;
}
void DVector::reDim(int newdim) {}
int main() {}

