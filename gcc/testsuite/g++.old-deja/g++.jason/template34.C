template<class T>
class Set {
  public:
    typedef int (*Compare)(const T&, const T&);
    static Compare cmp1;
    static int (*cmp2)(const T&, const T&);
};

template<class T>
int gen_cmp(const T& a, const T& b) {
    if (a<b) return -1;
    else if (a==b) return 0;
    else return 1;
}

template<class T>
Set<T>::Compare Set<T>::cmp1 = &gen_cmp;

template<class T>
int (*Set<T>::cmp2)(const T&, const T&) = &gen_cmp;

int main() {
    Set<int> s;
}
