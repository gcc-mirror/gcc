// PR c++/86521
// { dg-do compile { target c++11 } }

template <class T> T&& move (T&);

struct Dest {
  Dest() = default;
  Dest( Dest && ) = default;
  Dest( Dest const & ) = delete;
};

struct Source {
  Dest val;
  operator Dest () && { return move( val ); }
  operator Dest const & () const & { return val; }
};

int main() {
  Source x;
  Dest d(move(x));	   // { dg-error "ambiguous" "" { target c++14_down } }
}
