// PR optimization/6713
// This testcase segfaulted on x86 because a dangling REG_EQUAL note
// resulted in incorrect substitutions later.
// { dg-do run }
// { dg-options "-O2" }

template<typename _CharT> class basic_iterator
{
  public:
    basic_iterator(_CharT* _p) : _M_current(_p) {}
    basic_iterator& operator++() { ++_M_current; return *this; }
    _CharT& operator*() const { return *_M_current; }
    bool operator!=(basic_iterator &_rhs) { return _M_current != _rhs._M_current; }

  private:
    _CharT* _M_current;
};

template<typename _CharT> class basic_string
{
  public:
    typedef unsigned int size_type;
    typedef basic_iterator<_CharT> iterator;

  private:
    struct _Rep
    {
      size_type _M_length;
      size_type _M_capacity;
      int _M_references;

      bool _M_is_leaked() const { return _M_references < 0; }
      bool _M_is_shared() const { return _M_references > 0; }
      void _M_set_leaked() { _M_references = -1; }
      void _M_set_sharable() { _M_references = 0; }
    };

    struct _Rep _M_rep;

    struct _Alloc_hider
    {
      _CharT _raw[16];
      _CharT* _M_p;
    };

    mutable _Alloc_hider _M_dataplus;

    _CharT* _M_data() const { return _M_dataplus._M_p; }

    void _M_leak() { if (!_M_rep._M_is_leaked()) _M_leak_hard(); }

    static int count;

    static void _M_leak_hard();

  public:
    explicit basic_string(const _CharT* __s);

    iterator begin() { _M_leak(); return iterator(_M_data()); }

    iterator end() { _M_leak(); return iterator(_M_data() + this->size()); }

    size_type size() const { return _M_rep._M_length; }
};

template<typename _CharT> basic_string<_CharT>::
basic_string(const _CharT* __s)
{
  int i;

  for (i=0; i<15; i++) {
    if (!__s[i])
      break;

    _M_dataplus._raw[i] = __s[i];
  }

  _M_dataplus._raw[i] = 0;
  _M_dataplus._M_p = _M_dataplus._raw;

  _M_rep._M_length = i;
  _M_rep._M_capacity = i;
  _M_rep._M_references = 1;
}     

template<typename _CharT> int basic_string<_CharT>::count = 0;

template<typename _CharT> void basic_string<_CharT>::
_M_leak_hard()
{
  count++;
}

typedef basic_string<char> string;

template int basic_string<char>::count;

int isspa(int ch)
{
  return 0;
}

void foo(string& str)
{
  string::iterator it = str.begin();
  string::iterator stop = str.end();

  for (; it != stop; ++it)
    if (isspa(*it))
      break;
}

int main()
{
  string str("test");
  foo(str);
}
