template <typename T>
class foo
{
  T t;
};

class bar;
typedef foo<bar> foobar;

class obj
{
  virtual foobar* yeah() = 0;
};

class bar : virtual public obj
{
};
