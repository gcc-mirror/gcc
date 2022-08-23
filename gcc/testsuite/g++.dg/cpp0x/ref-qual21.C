// PR c++/104173
// { dg-do compile { target c++11 } }

struct QString {
  QString toLower() &&;
};

struct QCoreApplication {
  static QString applicationName();
};

QCoreApplication* instance();

void f() {
  instance()->applicationName().toLower();
}

template<class...>
void g() {
  instance()->applicationName().toLower();
}

template void g<>();
