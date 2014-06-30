// PR c++/33972

struct s
{
  typedef void f(void);
  f operator();
};
