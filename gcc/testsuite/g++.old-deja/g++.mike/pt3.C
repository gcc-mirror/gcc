// { dg-do assemble  }

template<class Repr>
class syHandle
{
protected:
  syHandle();
  ~syHandle();
  Repr *_repr;
};

template<class Repr>
syHandle<Repr>::~syHandle()
{
}

typedef char * char_ptr_t;

template <>
syHandle<char_ptr_t>::syHandle() 
{
    _repr = 0;
}

template <>
syHandle<char_ptr_t>::~syHandle() 
{
    _repr = 0;
}
