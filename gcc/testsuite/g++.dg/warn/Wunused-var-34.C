// PR c++/85963
// { dg-additional-options -Wall }

template<typename T>
struct foo {
  T val, alpha;
  foo() : val(0), alpha(0) {}
};

template<typename T>
inline void bar(const foo<T>& A, const foo<T>& B, foo<T>& C) {
  const bool use_alpha = true;
  const T        alpha = use_alpha ? (A.alpha * B.alpha) : T(0);
  
  C.val   = A.val * B.val;
  C.alpha = alpha;
}


int main() {
  foo<double> A,B,C;
  
  bar(A,B,C);
  
  return 0;
}

