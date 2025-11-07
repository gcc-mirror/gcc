// { dg-do compile }
// { dg-additional-options "-g" }

struct QPointF {
  QPointF(double xpos, double ypos) : xp(xpos), yp(ypos) {}
  double xp;
  double yp;
};
double xp, yp, w, h;
struct QRectF {
  QRectF(QPointF, int);
  QPointF center() { return QPointF(xp + w / 2, yp + h / 2); }
};
void clientArea(QPointF &);
int workspace_size;
void workspace() {
  QRectF geom(QPointF(0, 0), workspace_size);
  xp = 0 - w / 2;
  yp = -h;
  QPointF __trans_tmp_2 = geom.center();
  clientArea(__trans_tmp_2);
}
