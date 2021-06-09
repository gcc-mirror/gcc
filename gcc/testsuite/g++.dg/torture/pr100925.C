// { dg-do compile }

struct QScopedPointerDeleter {
  static void cleanup(int *);
};
class QScopedPointer {
  typedef int *QScopedPointer::*RestrictedBool;

public:
  operator RestrictedBool() { return d ? nullptr : &QScopedPointer::d; }
  void reset() {
    if (d)
      QScopedPointerDeleter::cleanup(d);
  }
  int *d;
};
class DOpenGLPaintDevicePrivate {
public:
  QScopedPointer fbo;
} DOpenGLPaintDeviceresize_d;
void DOpenGLPaintDeviceresize() {
  if (DOpenGLPaintDeviceresize_d.fbo)
    DOpenGLPaintDeviceresize_d.fbo.reset();
}
