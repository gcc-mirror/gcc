struct B {
  enum class E { V0, V1 };
  virtual ~B();
  E e;
};

B::~B() = default;


