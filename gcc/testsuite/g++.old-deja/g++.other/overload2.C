// { dg-do link  }

template <class T>
class ConstArray {
};

template <class T1, class T2>
void operator+(const ConstArray<T1>&, const ConstArray<T2>&)
{
}

template <class T1, class T2>
void operator+(const ConstArray<T1>&, T2);

template <class T1, class T2>
void operator+(T1, const ConstArray<T2>&);

const ConstArray<int> cai() { return ConstArray<int>(); }
const ConstArray<double> cad() { return ConstArray<double>(); }

int main()
{
  cai () + cad ();
}
