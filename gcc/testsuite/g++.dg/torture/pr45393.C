// { dg-do compile }

class FloatPoint;
class Path {
public:
    ~Path();
    void moveTo(const FloatPoint&);
    static void createEllipse(const FloatPoint& center, float rx, float ry);
};
extern "C" {
    extern float cosf (float);
    extern float sinf (float);
}
const float piFloat = static_cast<float>(3.14159265358979323846);
class FloatPoint {
public:
    FloatPoint(float x, float y) : m_x(x), m_y(y) { }
    float x() const;
    float y() const;
    float m_x, m_y;
};
void Path::createEllipse(const FloatPoint& center, float rx, float ry)
{
  float cx = center.x();
  float cy = center.y();
  Path path;
  float x = cx;
  float y = cy;
  unsigned step = 0, num = 100;
  while (1) {
      float angle = static_cast<float>(step) / num * 2.0f * piFloat;
      x = cx + cosf(angle) * rx;
      y = cy + sinf(angle) * ry;
      step++;
      if (step == 1)
	path.moveTo(FloatPoint(x, y));
  }
}
