// Build don't link:

template<class X, class Z>
class foo
{
public:
  typedef X y;

  class bar {
  public:
    void blah () { y Y; }
  };
};
