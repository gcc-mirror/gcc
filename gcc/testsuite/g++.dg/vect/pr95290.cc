// { dg-do compile }

typedef short a;
enum b {};
typedef struct {
      a c;
} d;
typedef struct {
      a e, f;
} g;
typedef struct {
      g h;
} i;
typedef struct {
      d j;
        int k;
} l;
class m
{
  i imgdata;
  void n();
  l o;
};
void m::n() try { imgdata.h.e = imgdata.h.f = o.j.c >> o.j.c; } catch (b) {
}
