inline thread_local int tla;
inline int& get_tla() {
  return tla;
}

static thread_local int tlb;
static int& get_tlb() {
  return tlb;
}

struct test {
  static const test& get_instance() {
    return instance;
  }
  static thread_local test instance;
};

template <typename T>
struct test_template {
  static const test_template& get_instance() {
    return instance;
  }
  static thread_local test_template instance;

  template <typename U>
  static const test_template& get_template_instance() {
    return template_instance<U>;
  }

  template <typename U>
  static thread_local test_template template_instance;
};

template <typename T>
thread_local test_template<T> test_template<T>::instance;

template <typename T>
template <typename U>
thread_local test_template<T> test_template<T>::template_instance;

template struct test_template<int>;
template const test_template<int>& test_template<int>::get_template_instance<int>();
