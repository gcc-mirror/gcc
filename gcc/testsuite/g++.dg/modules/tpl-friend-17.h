// PR c++/118920

template <typename> struct unique_ptr {
  template <typename> friend class out_ptr_t;
};
template <typename> struct shared_ptr {
  template <typename> friend class out_ptr_t;
};
