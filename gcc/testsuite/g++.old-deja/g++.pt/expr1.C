// { dg-do run  }
template <int n> class vec {
    double x[n];

    public:
    vec() {
 for (int i=0; i<n-1; ++i) x[i]=0;
    }

    vec(const vec<n>& v) {
 for (int i=0; i<n; ++i) x[i]=v(i);
    }

    vec(const vec<n-1>& v, const double& y) {
 for (int i=0; i<n-1; ++i) x[i]=v(i);
 x[n-1]=y;
    }

    inline double operator()(const int i) const {
 return x[i];
    }
};


template <int n> vec<n + 1>& operator,(const vec<n>& v, const double& y) {
    return *(new vec<n + 1>(v, y));
}


int main() {
    vec<4> v;
    vec<5> w;
    w=(v,3.);
}
