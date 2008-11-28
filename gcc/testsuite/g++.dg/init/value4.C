// PR c++/38233

template<class _T1, class _T2>
  struct pair
  {
    _T1 first;
    _T2 second;

    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 265.  std::pair::pair() effects overly restrictive
    /** The default constructor creates @c first and @c second using their
     *  respective default constructors.  */
    pair()
    : first(), second() { }
};

class a {
 public:
  a();
};

class b {
 public:
  // implicit default ctor
  bool operator<(const b& rhs) const;

 private:
  a a_val;
};

typedef pair<const b, int> my_pair;

void func() {
  my_pair x;
}
