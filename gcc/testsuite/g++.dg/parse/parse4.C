/* PR c++/88 */
/* { dg-do compile } */

class Hvec
{
public:
  Hvec(double x, double y, double z);
  ~Hvec();
};


class Camera1
{
public:
  Camera1(const Hvec &basepos=Hvec(0.0,0.0,0.0));
  ~Camera1();
};

class Camera2
{
public:
  Camera2(const Hvec &basepos);
  inline Camera2() { Camera2(Hvec(0.0,0.0,0.0)); }
  ~Camera2();
};
