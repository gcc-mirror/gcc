// { dg-options "-frepo -DF='a'" }

template <typename A, typename B> void f () {}
template <typename A, typename B> void g () { f<int,int>(); }
int main () { g<int,int>(); }

char c = F;
