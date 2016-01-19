// PR c++/24663

template<int I> int f1 (char[I]);
template<int I> int f1 (char p1 = I);
int i = f1<0>(0);

template<typename T, int I> int f2 (T[I]); // { dg-error "" }
int j = f2<int, 0>(0); // { dg-error "no matching function" }
int k = f2<void, 1>(0); // { dg-error "no matching function" }

int o[5];
int l = f2<int[5], 1>(&o);

template<int I> int f3 (char [][I]);
template<int I> int f3 (char p1 = I);
int x1 = f3<1>(0); // { dg-error "is ambiguous" }
int x2 = f3<1>();

template<typename T, int I> int f4 (T [][I]); // { dg-error "" }
int y1 = f4<void, 1>(0); // { dg-error "no matching function" }
int y2 = f4<int (void), 1>(0); // { dg-error "no matching function" }
int y3 = f4<int&, 1>(0); // { dg-error "no matching function" }
