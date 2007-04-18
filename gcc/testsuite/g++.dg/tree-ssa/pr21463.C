/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1" } */

template<class T> static inline const T &ref_max(const T &a, const T &b)
{ return a<b ? b : a; }
template<class T> static inline const T &ref_min(const T &a, const T &b)
{ return a<b ? a : b; }

template<class T> struct foo_t {
	T a0, a1;
	T bar_ref(const T b, const T c) {
		return ref_max(ref_min(a0, c), ref_min(ref_max(a1, c), b));
	}
};

template struct foo_t<int>;

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "phiopt1" } } */
/* { dg-final { cleanup-tree-dump "phiopt1" } } */
