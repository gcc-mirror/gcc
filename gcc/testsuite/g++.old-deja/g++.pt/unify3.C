// { dg-do assemble  }

template <class T1, class T2>
struct ComputeBinaryType
{
};

template<class T1>
struct ComputeBinaryType<T1, double> {
  void g();
};

template<class T1>
struct ComputeBinaryType<T1&, double> {
  void h();
};

void f()
{ 
  ComputeBinaryType<double, double> cb;
  cb.g();
}
