// Build don't link:
// Origin: David Mazieres <dm@amsterdam.lcs.mit.edu>

template<class T> struct vector_base {
  typedef T elm_t;
protected:
  union {
    double alignment_hack;
    char defbuf_space[2 * sizeof (elm_t)];
  };
  elm_t *def_basep () { return reinterpret_cast<elm_t *> (defbuf_space); }
};

template<class T> struct vector : public vector_base<T> {
  vector () { def_basep (); }
};

vector<int> iv;
