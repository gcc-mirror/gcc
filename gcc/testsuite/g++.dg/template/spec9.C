// { dg-do compile }

// Origin: Lynn Akers <lakers@peachtree.com>
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/10956: Incorrect template substitution for member template
// specialization inside template class.

template <int> struct C {
    template<typename T> void pre_add(T);
};

template<>
template<typename T>
void C<32>::pre_add(T) {
  T pre;
}

int main() {
  C<32>().pre_add<int>(1);
}
