/* { dg-do compile } */

typedef __SIZE_TYPE__ a;
extern "C" void *memset(void *, int, a);
typedef struct b c;
enum d { e };
template <int, typename> class f {
public:
    template <typename g> f(g);
};
typedef f<1, long> h;
template <typename> struct j {
    enum k {};
};
class l {
public:
    typedef j<l>::k k;
    l(k);
    operator d();
};
struct b {};
class m {};
c q(h, d);
c n(unsigned char o[]) {
    int i;
    long r;
    for (i = 0; i < 4; i++)
      r = o[i];
    return q(r, l((l::k)e));
}
m p() {
    unsigned char o[4], s = 1;
    for (;;) {
	memset(o, s, 4);
	n(o);
	s = 2;
    }
}
