// PR c++/61148
// { dg-do compile { target c++11 } }

class DB
{
protected:
  void foo() {};
};

class DC : public DB
{
public:
  DC()
  {
    [this]() {DB::foo();}();
  };
};

template <class T>
class DC2 : public T
{
public:
  DC2()
  {
    [this]() {T::foo();}();
  };
};

int main(void)
{
  DC x;
  DC2<DB> x2;
}
