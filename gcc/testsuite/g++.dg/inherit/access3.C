// { dg-do compile }

class __new_alloc {
public:
  static void allocate() {}
};

template <class _Alloc>
class __debug_alloc : public _Alloc {
public:
  static void allocate();
};

template <class _Alloc>
void __debug_alloc<_Alloc>::allocate() {
  _Alloc::allocate();
}

template class __debug_alloc<__new_alloc>;
