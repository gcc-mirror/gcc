// PR c++/97186
// ICE in exception spec substitution


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
