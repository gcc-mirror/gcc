// PR 97395
// ICE injecting hidden decl in wrong namespace

namespace pr {
  template<typename WW>
  void
  kp ()
  {
    extern WW hz;
  }

  void
  n5 ()
  {
    kp<int[]> ();
    kp<int[1]> ();
  }
}
