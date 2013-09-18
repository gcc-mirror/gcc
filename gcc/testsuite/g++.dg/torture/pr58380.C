// { dg-do compile }
// { dg-options "-w" }

class iplugin_factory;
class idocument_plugin_factory {
  virtual idocument_plugin_factory *create_plugin(iplugin_factory &, int &);
};
template <typename _Iterator, typename> class __normal_iterator {
  _Iterator _M_current;

public:
  _Iterator iterator_type;
  __normal_iterator(const _Iterator &p1) : _M_current(p1) {}
  void operator++();
  _Iterator &base() { return _M_current; }
};

template <typename _IteratorL, typename _IteratorR, typename _Container>
int operator!=(__normal_iterator<_IteratorL, _Container> &p1,
               __normal_iterator<_IteratorR, _Container> &p2) {
  return p1.base() != p2.base();
}

class new_allocator {
public:
  typedef int *const_pointer;
  int *allocate();
};
template <typename> class allocator : public new_allocator {};

class basic_string {
public:
  basic_string(char *);
};
struct __uninitialized_copy {
  template <typename _InputIterator, typename _ForwardIterator>
  static _ForwardIterator __uninit_copy(_InputIterator p1, _InputIterator p2,
                                        _ForwardIterator p3) try {
    for (; p1 != p2; ++p1, ++p3)
      ;
    return p3;
  }
  catch (...) {
  }
};

template <typename _InputIterator, typename _ForwardIterator>
_ForwardIterator uninitialized_copy(_InputIterator p1, _InputIterator p2,
                                    _ForwardIterator p3) {
  return __uninitialized_copy::__uninit_copy(p1, p2, p3);
}

template <typename _InputIterator, typename _ForwardIterator, typename _Tp>
_ForwardIterator __uninitialized_copy_a(_InputIterator p1, _InputIterator p2,
                                        _ForwardIterator p3, allocator<_Tp> &) {
  return uninitialized_copy(p1, p2, p3);
}

struct _Vector_base {
  struct _Vector_impl : allocator<int> {
    int *_M_start;
    int *_M_finish;
  };
  allocator<int> &_M_get_Tp_allocator() {}
  _Vector_base() {}
  _Vector_base(int p1) { _M_create_storage(p1); }
  _Vector_impl _M_impl;
  int *_M_allocate(int p1) { p1 ? _M_impl.allocate() : 0; }
  void _M_create_storage(int p1) {
    this->_M_impl._M_start = this->_M_allocate(p1);
  }
};

class vector : _Vector_base {
  _Vector_base _Base;

public:
  vector() {}
  vector(const vector &p1) : _Base(p1.size()) {
    this->_M_impl._M_finish = __uninitialized_copy_a(
        p1.begin(), p1.end(), this->_M_impl._M_start, _M_get_Tp_allocator());
  }
  ~vector();
  __normal_iterator<typename allocator<int>::const_pointer, int> begin() const {
    return this->_M_impl._M_start;
  }
  __normal_iterator<typename allocator<int>::const_pointer, int> end() const {
    return this->_M_impl._M_finish;
  }
  int size() const { return this->_M_impl._M_finish - this->_M_impl._M_start; }
};
class iplugin_factory {
public:
  typedef enum {
    STABLE,
    EXPERIMENTAL
  } quality_t;
};
class plugin_factory : public iplugin_factory {
public:
  plugin_factory(const int &, const basic_string &, const basic_string &,
                 const basic_string &, quality_t);
};
template <typename plugin_t>
class document_plugin_factory : plugin_factory, idocument_plugin_factory {
public:
  document_plugin_factory(const int &p1, const basic_string &,
                          const basic_string &, const basic_string &, quality_t)
      : plugin_factory(0, 0, 0, 0, STABLE) {}
  idocument_plugin_factory *create_plugin(iplugin_factory &p1, int &p2) {
    plugin_t(p1, p2);
  }
};

class container {
public:
  template <typename init_t> container(init_t &);
};
template <class init_t> class initializer_t : init_t {
public:
  initializer_t(const init_t &p1) : init_t(p1) {}
};

class composition_t {};
template <typename lhs_t, typename rhs_t>
const initializer_t<composition_t> operator+(const initializer_t<lhs_t> &,
                                             const initializer_t<rhs_t> &);
template <typename value_t> class value_initializer_t {
public:
  value_initializer_t(const value_t &p1) : m_value(p1) {}
  value_t m_value;
};

template <typename value_t>
initializer_t<value_initializer_t<value_t> > init_value(const value_t &p1) {
  initializer_t<value_initializer_t<value_t> >(
      value_initializer_t<value_t>(p1));
}

class name_t {};
class label_t {};
class description_t {};
class owner_initializer_t {};
template <typename owner_t>
initializer_t<owner_initializer_t> init_owner(owner_t &);
class set : vector {};
class node {
public:
  node(iplugin_factory &, int &);
};
initializer_t<name_t> init_name();
initializer_t<label_t> init_label();
initializer_t<description_t> init_description();
template <typename base_t> class mesh_selection_sink : base_t {
public:
  mesh_selection_sink(iplugin_factory &p1, int &p2)
      : base_t(p1, p2),
        m_mesh_selection(init_owner(*this) + init_name() + init_label() +
                         init_description() + init_value(set())) {}
  container m_mesh_selection;
};

class selection_to_stdout : mesh_selection_sink<node> {
public:
  selection_to_stdout(iplugin_factory &p1, int &p2)
      : mesh_selection_sink(p1, p2) {}
  static iplugin_factory &get_factory() {
    document_plugin_factory<selection_to_stdout>(0, "", 0, "",
                                                 iplugin_factory::EXPERIMENTAL);
  }
};

void selection_to_stdout_factory() { selection_to_stdout::get_factory(); }
