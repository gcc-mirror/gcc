// PR c++/38380
// { dg-options "-std=gnu++0x" }

namespace std
{
 struct atomic_bool
  {
    bool _M_i;

    atomic_bool() = default;
    ~atomic_bool() = default;
    atomic_bool(const atomic_bool&) = delete;
    atomic_bool& operator=(const atomic_bool&) = delete;

    explicit atomic_bool(bool __i) { _M_i = __i; }

    operator bool() const volatile
    { return true; }
  };
}

namespace __gnu_test
{
  struct direct_list_initializable
  {
    template<typename _Ttype, typename _Tvalue>
      void 
      operator()()
      {
        struct _Concept
        {
          void __constraint()
          { 
            _Ttype __v1 = { }; // default ctor
            _Ttype __v2 { __a };  // single-argument ctor
          }

          _Tvalue __a;
        };

        void (_Concept::*__x)() __attribute__((unused))
          = &_Concept::__constraint;
      }
  };
}

int main()
{
  __gnu_test::direct_list_initializable test;

  test.operator()<std::atomic_bool, bool>();
  return 0;
}
