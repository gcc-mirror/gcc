// PR c++/57550

template <bool (double)> bool Wrapper(double);
template <class T> void MakeHandler(bool (T));

class Handler
{
public:
  template <typename T> static void SetPrimitiveHandlers()
  {
    MakeHandler(Wrapper<Append<T> >);
  }
private :
  template <typename T> static bool Append(T);
};

template void Handler::SetPrimitiveHandlers<double>();
