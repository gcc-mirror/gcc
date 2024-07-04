// PR c++/115159
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

struct QDebug;

template<class T> void f(T);

template<class T> struct QList {
  QDebug g(QList &other) noexcept(noexcept(f(other)));
  QDebug h(QList &other) noexcept(noexcept(f(other)));
};

struct QObjectData {
  QList<int> children;
};

struct QIODevice {
  QObjectData d_ptr;
};

struct QDebug {
  QDebug(QIODevice);
};
