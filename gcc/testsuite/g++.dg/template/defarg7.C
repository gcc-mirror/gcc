// PR c++/25337

template <typename T> T& MakeT();
template <typename U, int N = sizeof (MakeT<U>().operator[](0))>
struct helper{};
template <typename U>
static char is_here(helper<U>*);
