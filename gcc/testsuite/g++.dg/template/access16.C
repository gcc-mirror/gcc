// PR c++/23842

struct S;
extern S *p;
template <class T> int f(T*, int y = ((T*)p)->x) {
	return y;
}
struct S {
private:
  int x;
  template <class U> friend int f(U*, int);
};
int g() {
  return f(p);
}
			   
