// Build don't link:
// Origin: Theodore Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

template <typename T>
struct foo {
    enum { A = 4 >= 4, B = (1 ? true : A) };
};
 
foo<int> bar;
