// { dg-do assemble  }

template<unsigned int n> struct PartialDotProduct {
    template<class T>
    static T Expand(T* a, T* b) { return T(); }
};

const int N = 10;

template<class In1, class In2>
void
dot(In1 f1, In2 f2)
{
  PartialDotProduct<N>::Expand(f1, f2);

}

int main()
{
  double a[N], b[N];
  
  dot(&a[0], &b[0]);
}
