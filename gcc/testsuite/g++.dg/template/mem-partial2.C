// PR c++/14032
// { dg-do run }

template <bool compare>
struct outer
{
  template <bool compare_with,bool second>
  struct inner           // unspecialized compare != compare_with
  {
    static inline bool test()
    {
      return false;
    }
  };
  template <bool second> // specialization compare == compare_with
  struct inner<compare,second>
  {
    static inline bool test()
    {
      return true;
    }
  };
};
int main ()
{
  bool b = outer<true>::inner<true,false>::test();

  return b != true;
}
