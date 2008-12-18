// { dg-do run }
// { dg-options "-fpic" { target fpic } }

typedef __SIZE_TYPE__ size_t;

template <typename NumType>
inline
NumType
absolute(NumType const& x)
{
  if (x < NumType(0)) return -x;
  return x;
}

class trivial_accessor
{
  public:
    typedef size_t index_type;
    struct index_value_type {};

    trivial_accessor() : size_(0) {}

    trivial_accessor(size_t const& n) : size_(n) {}

    size_t size_1d() const { return size_; }

  protected:
    size_t size_;
};

namespace N0
{
  template <typename ElementType,
            typename AccessorType = trivial_accessor>
  class const_ref
  {
    public:
      typedef ElementType value_type;
      typedef size_t size_type;

      typedef AccessorType accessor_type;
      typedef typename accessor_type::index_type index_type;
      typedef typename accessor_type::index_value_type index_value_type;

      const_ref() {}

      const_ref(const ElementType* begin, accessor_type const& accessor)
      : begin_(begin), accessor_(accessor)
      {
        init();
      }

      const_ref(const ElementType* begin, index_value_type const& n0)
      : begin_(begin), accessor_(n0)
      {
        init();
      }

      const_ref(const ElementType* begin, index_value_type const& n0,
                                          index_value_type const& n1)
      : begin_(begin), accessor_(n0, n1)
      {
        init();
      }

      const_ref(const ElementType* begin, index_value_type const& n0,
                                          index_value_type const& n1,
                                          index_value_type const& n2)
      : begin_(begin), accessor_(n0, n1, n2)
      {
        init();
      }

      accessor_type const& accessor() const { return accessor_; }
      size_type size() const { return size_; }

      const ElementType* begin() const { return begin_; }
      const ElementType* end() const { return end_; }

      ElementType const&
      operator[](size_type i) const { return begin_[i]; }

      const_ref<ElementType>
      as_1d() const
      {
        return const_ref<ElementType>(begin_, size_);
      }

    protected:
      void
      init()
      {
        size_ = accessor_.size_1d();
        end_ = begin_ + size_;
      }

      const ElementType* begin_;
      accessor_type accessor_;
      size_type size_;
      const ElementType* end_;
  };
}

template <typename ElementType,
          typename AccessorType = trivial_accessor>
class ref : public N0::const_ref<ElementType, AccessorType>
{
  public:
    typedef ElementType value_type;
    typedef size_t size_type;

    typedef N0::const_ref<ElementType, AccessorType> base_class;
    typedef AccessorType accessor_type;
    typedef typename accessor_type::index_type index_type;

    ref() {}

    ElementType*
    begin() const { return const_cast<ElementType*>(this->begin_); }

    ElementType*
    end() const { return const_cast<ElementType*>(this->end_); }

    ElementType&
    operator[](size_type i) const { return begin()[i]; }
};

namespace N1 {
  template <typename ElementType, size_t N>
  class tiny_plain
  {
    public:
      typedef ElementType value_type;
      typedef size_t size_type;

      static const size_t fixed_size=N;

      ElementType elems[N];

      tiny_plain() {}

      static size_type size() { return N; }

      ElementType* begin() { return elems; }
      const ElementType* begin() const { return elems; }
      ElementType* end() { return elems+N; }
      const ElementType* end() const { return elems+N; }
      ElementType& operator[](size_type i) { return elems[i]; }
      ElementType const& operator[](size_type i) const { return elems[i]; }
  };

  template <typename ElementType, size_t N>
  class tiny : public tiny_plain<ElementType, N>
  {
    public:
      typedef ElementType value_type;
      typedef size_t size_type;

      typedef tiny_plain<ElementType, N> base_class;

      tiny() {}
  };
}

template <typename NumType>
class mat3 : public N1::tiny_plain<NumType, 9>
{
  public:
    typedef typename N1::tiny_plain<NumType, 9> base_type;

    mat3() {}
    mat3(NumType const& e00, NumType const& e01, NumType const& e02,
         NumType const& e10, NumType const& e11, NumType const& e12,
         NumType const& e20, NumType const& e21, NumType const& e22)
      : base_type(e00, e01, e02, e10, e11, e12, e20, e21, e22)
    {}
    mat3(base_type const& a)
      : base_type(a)
    {}

    NumType const&
    operator()(size_t r, size_t c) const
    {
      return this->elems[r * 3 + c];
    }
    NumType&
    operator()(size_t r, size_t c)
    {
      return this->elems[r * 3 + c];
    }

    NumType
    trace() const
    {
      mat3 const& m = *this;
      return m[0] + m[4] + m[8];
    }

    NumType
    determinant() const
    {
      mat3 const& m = *this;
      return   m[0] * (m[4] * m[8] - m[5] * m[7])
             - m[1] * (m[3] * m[8] - m[5] * m[6])
             + m[2] * (m[3] * m[7] - m[4] * m[6]);
    }
};

template <typename NumType>
inline
mat3<NumType>
operator-(mat3<NumType> const& v)
{
  mat3<NumType> result;
  for(size_t i=0;i<9;i++) {
    result[i] = -v[i];
  }
  return result;
}

class mat_grid : public N1::tiny<size_t, 2>
{
  public:
    typedef N1::tiny<size_t, 2> index_type;
    typedef index_type::value_type index_value_type;

    mat_grid() { this->elems[0]=0; this->elems[1]=0; }

    mat_grid(index_type const& n) : index_type(n) {}

    mat_grid(index_value_type const& n0, index_value_type const& n1)
    { this->elems[0]=n0; this->elems[1]=n1; }

    size_t size_1d() const { return elems[0] * elems[1]; }

    size_t
    operator()(index_value_type const& r, index_value_type const& c) const
    {
      return r * elems[1] + c;
    }
};

template <typename NumType, typename AccessorType = mat_grid>
class mat_const_ref : public N0::const_ref<NumType, AccessorType>
{
  public:
    typedef AccessorType accessor_type;
    typedef typename N0::const_ref<NumType, AccessorType> base_type;
    typedef typename accessor_type::index_value_type index_value_type;

    mat_const_ref() {}

    mat_const_ref(const NumType* begin, accessor_type const& grid)
    : base_type(begin, grid)
    {}

    mat_const_ref(const NumType* begin, index_value_type const& n_rows,
                                        index_value_type const& n_columns)
    : base_type(begin, accessor_type(n_rows, n_columns))
    {}

    accessor_type
    grid() const { return this->accessor(); }

    index_value_type const&
    n_rows() const { return this->accessor()[0]; }

    index_value_type const&
    n_columns() const { return this->accessor()[1]; }

    NumType const&
    operator()(index_value_type const& r, index_value_type const& c) const
    {
      return this->begin()[this->accessor()(r, c)];
    }
};

template <typename NumType, typename AccessorType = mat_grid>
class mat_ref : public mat_const_ref<NumType, AccessorType>
{
  public:
    typedef AccessorType accessor_type;
    typedef mat_const_ref<NumType, AccessorType> base_type;
    typedef typename accessor_type::index_value_type index_value_type;

    mat_ref() {}

    mat_ref(NumType* begin, accessor_type const& grid)
    : base_type(begin, grid)
    {}

    mat_ref(NumType* begin, index_value_type n_rows,
                            index_value_type n_columns)
    : base_type(begin, accessor_type(n_rows, n_columns))
    {}

    NumType*
    begin() const { return const_cast<NumType*>(this->begin_); }

    NumType*
    end() const { return const_cast<NumType*>(this->end_); }

    NumType&
    operator[](index_value_type const& i) const { return begin()[i]; }

    NumType&
    operator()(index_value_type const& r, index_value_type const& c) const
    {
      return this->begin()[this->accessor()(r, c)];
    }
};

  template <typename AnyType>
  inline void
  swap(AnyType* a, AnyType* b, size_t n)
  {
    for(size_t i=0;i<n;i++) {
      AnyType t = a[i]; a[i] = b[i]; b[i] = t;
    }
  }

template <typename IntType>
size_t
form_t(mat_ref<IntType>& m,
       mat_ref<IntType> const& t)
{
  typedef size_t size_t;
  size_t mr = m.n_rows();
  size_t mc = m.n_columns();
  size_t tc = t.n_columns();
  if (tc) {
  }
  size_t i, j;
  for (i = j = 0; i < mr && j < mc;) {
    size_t k = i; while (k < mr && m(k,j) == 0) k++;
    if (k == mr)
      j++;
    else {
      if (i != k) {
                swap(&m(i,0), &m(k,0), mc);
        if (tc) swap(&t(i,0), &t(k,0), tc);
      }
      for (k++; k < mr; k++) {
        IntType a = absolute(m(k, j));
        if (a != 0 && a < absolute(m(i,j))) {
                  swap(&m(i,0), &m(k,0), mc);
          if (tc) swap(&t(i,0), &t(k,0), tc);
        }
      }
      if (m(i,j) < 0) {
                for(size_t ic=0;ic<mc;ic++) m(i,ic) *= -1;
        if (tc) for(size_t ic=0;ic<tc;ic++) t(i,ic) *= -1;
      }
      bool cleared = true;
      for (k = i+1; k < mr; k++) {
        IntType a = m(k,j) / m(i,j);
        if (a != 0) {
                  for(size_t ic=0;ic<mc;ic++) m(k,ic) -= a * m(i,ic);
          if (tc) for(size_t ic=0;ic<tc;ic++) t(k,ic) -= a * t(i,ic);
        }
        if (m(k,j) != 0) cleared = false;
      }
      if (cleared) { i++; j++; }
    }
  }
  m = mat_ref<IntType>(m.begin(), i, mc);
  return i;
}

template <typename IntType>
size_t
form(mat_ref<IntType>& m)
{
  mat_ref<IntType> t(0,0,0);
  return form_t(m, t);
}

typedef mat3<int> sg_mat3;

class rot_mx
{
  public:
    explicit
    rot_mx(sg_mat3 const& m, int denominator=1)
    : num_(m), den_(denominator)
    {}

    sg_mat3 const&
    num() const { return num_; }
    sg_mat3&
    num()       { return num_; }

    int const&
    operator[](size_t i) const { return num_[i]; }
    int&
    operator[](size_t i)       { return num_[i]; }

    int
    const& operator()(int r, int c) const { return num_(r, c); }
    int&
    operator()(int r, int c)       { return num_(r, c); }

    int const&
    den() const { return den_; }
    int&
    den()       { return den_; }

    rot_mx
    minus_unit_mx() const
    {
      rot_mx result(*this);
      for (size_t i=0;i<9;i+=4) result[i] -= den_;
      return result;
    }

    rot_mx
    operator-() const { return rot_mx(-num_, den_); }

    int
    type() const;

    int
    order(int type=0) const;

  private:
    sg_mat3 num_;
    int den_;
};

class rot_mx_info
{
  public:
    rot_mx_info(rot_mx const& r);

    int type() const { return type_; }

  private:
    int type_;
};

int rot_mx::type() const
{
  int det = num_.determinant();
  if (det == -1 || det == 1) {
    switch (num_.trace()) {
      case -3:                return -1;
      case -2:                return -6;
      case -1: if (det == -1) return -4;
               else           return  2;
      case  0: if (det == -1) return -3;
               else           return  3;
      case  1: if (det == -1) return -2;
               else           return  4;
      case  2:                return  6;
      case  3:                return  1;
    }
  }
  return 0;
}

int rot_mx::order(int type) const
{
  if (type == 0) type = rot_mx::type();
  if (type > 0) return  type;
  if (type % 2) return -type * 2;
                return -type;
}

rot_mx_info::rot_mx_info(rot_mx const& r)
: type_(r.type())
{
  if (type_ == 0) {
    return;
  }
  rot_mx proper_r = r;
  int proper_order = type_;
  // THE PROBLEM IS AROUND HERE
  if (proper_order < 0) {
    proper_order *= -1;
    proper_r = -proper_r; // THIS FAILS ...
  }
  if (proper_order > 1) {
    rot_mx rmi = proper_r.minus_unit_mx(); // ... THEREFORE WRONG HERE
    mat_ref<int> re_mx(rmi.num().begin(), 3, 3);
    if (form(re_mx) != 2) {
      type_ = 0;
    }
  }
}

int main()
{
  N1::tiny<int, 9> e;
  e[0] = 1; e[1] =  0; e[2] = 0;
  e[3] = 0; e[4] = -1; e[5] = 0;
  e[6] = 0; e[7] =  0; e[8] = 1;
  rot_mx r(e);
  rot_mx_info ri(r);
  if (ri.type() != -2)
    __builtin_abort ();
  return 0;
}
