// { dg-do assemble  }
template <class T>
struct X
{
    virtual void f(int) const;
};

template <class T>
struct Y: public X<T>
{
    virtual void f(int) const;
};

template <class T>
void Y<T>::f(int) const
{
}

template <>
void Y<bool>::f(int) const;
