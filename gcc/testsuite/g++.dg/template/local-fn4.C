// PR c++/97186
// ICE in exception spec substitution
// { dg-do compile { target c++11 } }

template <class GG>
struct no {
  static void
  tg ()
  {
    void
      hk () noexcept (tg); // { dg-error "convert" }

    hk ();
  }
};

void
os ()
{
  no<int> ().tg ();
}
