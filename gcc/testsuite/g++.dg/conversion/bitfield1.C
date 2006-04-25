// { dg-do run }
// { dg-options "-w" }

enum E { a, b = 1LL << 48 };
 
struct S {
  E e : 3;
};

S s;

int main () {
  if (sizeof (E) != sizeof (long long))
    return 1;
  if (sizeof (s.e + 3) != sizeof (long long))
    return 2;
}
