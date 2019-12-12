// { dg-do compile { target c++11 } }
// { dg-options "-O3" }

struct h {
    typedef int &c;
};
class i {
    struct j {
	using c = int *;
    };
    using as = j::c;
};
template <typename> class k {
public:
    using as = i::as;
    h::c operator[](long l) {
	k<int[]>::as d = 0;
	return d[l];
    }
};
class : public k<int[]> { } a;
long c, f;
void m()
{
  for (long b; b <= 6; b++)
    for (long g; g < b; g++) {
	unsigned long e = g;
	c = 0;
	for (; c < b; c++)
	  f = e >>= 1;
	a[g] = f;
    }
}
