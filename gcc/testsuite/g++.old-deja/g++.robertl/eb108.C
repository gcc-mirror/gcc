// Build don't link: 
class X
{
  public:
    virtual void f() const = 0;
};

template <class T>
class Y: public X
{
  public:
    virtual void f() const;
};

template <class T>
void Y<T>::f() const
{
}

template <>
void Y<bool>::f() const;
