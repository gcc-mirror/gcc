// PR c++/85558

template <typename T>
struct triggerBug {
  friend void doInitUser(bool = triggerBug::doInit);  // { dg-error "definition" }
  static bool doInit;
};

template <class T>
bool triggerBug<T>::doInit = true;

triggerBug<int> bug;
