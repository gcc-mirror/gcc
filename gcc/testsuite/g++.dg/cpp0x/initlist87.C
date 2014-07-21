// PR c++/61723
// { dg-do compile { target c++11 } }

namespace std {
  template < class > struct initializer_list // { dg-message "initializer_list" }
  {
#if BUG1
    int _M_len;
#endif
    const int *begin ();
    const int *end ();
  };
}

struct J
{
    J (const int &);
    template < typename InputIterator > J (InputIterator, InputIterator);
    J (std::initializer_list < int >p1):J (p1.begin (), p1.end ()) { }
};

struct L
{
    L ():dim (0) { }
    J dim;
};

void
fn1 ()
{
    L spec;
    spec.dim = { };
}

// { dg-prune-output "compilation terminated" }
