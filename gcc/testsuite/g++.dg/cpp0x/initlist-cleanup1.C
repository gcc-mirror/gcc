// PR c++/93345
// { dg-do compile { target c++11 } }

struct ln {
  ~ln ();
};

struct ry {
  ln kj;
};

template<typename GC>
void
dz ()
{
  ry{};
}
