// PR c++/48883
// { dg-do link }

template<typename T>
T myMax(T a, T b) {
   if(a < b) return a;
   return b;
}

int main() {
   bool even = true;
   int (*fp)(int, int);
   fp = even ? myMax<int> : myMax<int>;   /* yields link error */
}
