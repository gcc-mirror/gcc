// { dg-do run }
// { dg-additional-options "-fstack-protector-strong -fno-late-combine-instructions" }
// { dg-require-effective-target fstack_protector }
// { dg-require-effective-target c++11 }

struct Private {
  char data[24]{};
  long moved_from : 4;
  Private() : moved_from (0) {}
};

struct QVariant {
  __attribute__((noipa))
  ~QVariant() {
    if (!d.moved_from && d.data[0] != 42)
      __builtin_abort ();
  }
  __attribute__((noipa))
  QVariant() {
    d.data[0] = 42;
  }
  __attribute__((noipa))
  QVariant(QVariant &other) : d(other.d) {}
  QVariant(QVariant &&other) : d(other.d) {
    other.d = Private();
    other.d.moved_from = true;
  }
  QVariant &operator=(QVariant);
  Private d;
};

QVariant id (QVariant v) { return v; }
QVariant &QVariant::operator=(QVariant other)
{
  id(other);
  return *this;
}

template <typename T> struct QList {
  T d;
  struct const_iterator {
    T *ptr;
    T &operator*() { return *ptr; }
    __attribute__((noipa))
    bool operator!=(const_iterator other) { return ptr != other.ptr; }
    void operator++() { ptr++; }
  };
  __attribute__((noipa))
  T at() { return d; }
  const_iterator begin() { return const_iterator { &d }; }
  const_iterator end() { return const_iterator { &d }; }
};
struct QArrayDataPointer {
  int d;
  int *ptr;
  long size;
};

QArrayDataPointer null_qadp;

struct QString {
  __attribute__((noipa))
  QList<QString> split() const {
    return QList<QString> {null_qadp};
  }
  __attribute__((noipa))
  friend bool operator==(QString, QString) { return true; }

  __attribute__((noipa))
  QString(QArrayDataPointer dp) : d(dp) {}
  QArrayDataPointer d;
};

__attribute__((noipa))
QString as_qstr (QVariant *v)
{
  return QString (null_qadp);
}

int *getNode(const QString &tagContent) {
  auto expr = tagContent.split();
  auto blockName = expr.at();
  auto loadedBlocksVariant = QVariant ();
  QList<QVariant> blockVariantList;
  for (auto &item : blockVariantList) {
    auto blockNodeName = as_qstr (&item);
    blockNodeName == blockName;
    QString q(null_qadp);
  }
  loadedBlocksVariant = QVariant();
  return nullptr;
}

int main(void)
{
  QString foo(null_qadp);
  getNode(foo);
}
