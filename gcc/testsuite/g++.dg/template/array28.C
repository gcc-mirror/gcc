typedef int (A)[];

template<class T> void f(T (*)[1]); // { dg-error "array" }

int main() {
  f<int[]>(0);			// { dg-error "no match" }
}
