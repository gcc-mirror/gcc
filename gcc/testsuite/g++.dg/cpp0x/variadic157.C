// PR c++/58353
// { dg-do compile { target c++11 } }

template<class E, E V, int CNT>
struct seq_t
{
  template<E... Es> struct seq{};

  template<int N, E... Es>
  struct gen : gen<N - 1, V, Es...>{};

  template<E... Es>
  struct gen<0, Es...> : seq<Es...>{};

  struct bits_t{ E e[CNT]; };

  template<E... Es>
  static bits_t init(seq<Es...>) {return {{Es...}};}

  static bits_t init() {return init(gen<CNT>{});}
};

typedef seq_t<int, 123, 5> wow;

int main()
{
  wow::init();
}
