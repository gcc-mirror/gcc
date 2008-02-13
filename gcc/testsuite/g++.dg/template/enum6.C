// PR c++/34774

template<int shifts>
struct shift {
  enum {
    n0 = (unsigned)shifts,
    n = n0 ? 0 : n0,
    n_comp = -n
  } x;
};
