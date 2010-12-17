// { dg-do compile }
// { dg-options "-fprefetch-loop-arrays -w" }

class ggPoint3 {
public:
    ggPoint3();
    inline double &x() {
	return e[0];
    }
    inline double &y() {
	return e[1];
    }
    ggPoint3(const ggPoint3 &p);
    double e[3];
};
class ggBox3 {
public:
    ggPoint3 min() const;
};
class ggHAffineMatrix3;
ggPoint3 operator*(const ggHAffineMatrix3 &m, const ggPoint3 &v);
void foo (ggPoint3 *);
void SetMatrix(ggHAffineMatrix3& toworld, ggBox3& box)
{
  ggPoint3 p[2][2][2];
  int i, j, k;
  for (i = 0; i < 2; j++)
    for (k = 0; k < 2; k++)
      {
	if (i == 0)
	  p[i][j][k].x() = box.min().x();
	if (j == 0)
	  p[i][j][k].y() = box.min().y();
	p[i][j][k] = toworld * p[i][j][k];
      }
  foo (&p[0][0][0]);
}
