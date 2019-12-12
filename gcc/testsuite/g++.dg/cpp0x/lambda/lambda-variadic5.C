// PR c++/47226
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-original" }
// { dg-final { scan-tree-dump-times "::<lambda\\(\\)> \\(null\\)" 6 "original" } }

template<class T>
void print(const T&) {}

template<class... T>
void accept_all(T&&...){}

template<class... T>
void print_all(const T&... t)
{
  accept_all([&]()->int { print(t); return 0; }...);
  accept_all([&t]()->int { print(t); return 0; }...);
}

int main()
{
  print_all(1, true, 'a');
}
