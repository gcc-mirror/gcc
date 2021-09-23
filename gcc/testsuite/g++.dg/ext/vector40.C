// PR c++/97900
// { dg-options "-Wno-psabi -w" }

template<typename T>
T test(T __attribute__((vector_size(2 * sizeof(T)))) vec) {
    return vec[0] + vec[1];
}
typedef int v2si __attribute__((vector_size(2 * sizeof(int))));
int run(v2si vec) {
    return test<int>(vec);
}
