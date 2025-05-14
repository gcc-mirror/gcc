// PR c++/119687
// { dg-do compile { target c++17 } }

template <typename> class QFlagsStorage{};

template <typename Enum> struct QFlagsStorageHelper : QFlagsStorage<Enum>  {
  using QFlagsStorage<Enum>::QFlagsStorage;

public:
  QFlagsStorageHelper(Enum);
};

template <typename Enum> struct QFlags : public QFlagsStorageHelper<Enum> {
  using Base = QFlagsStorageHelper<Enum>;
  using Base::Base;
  QFlags(Enum);
};

void f(int flag) {
  QFlags{int{}};
}
