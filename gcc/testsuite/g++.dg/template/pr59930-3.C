// PR c++/59930

namespace NS {
  template<typename T> class Holder
  {
  private:
    void func();

    template<typename> friend class User;
  };

  template class Holder<long>;

  template<typename T> class User
  {
  public:
    void method() const
    {
      Holder<T> x;
      x.func();
    }
  };
} // namespace

void Foo()
{
  NS::User<long> decl;
  decl.method();
}
