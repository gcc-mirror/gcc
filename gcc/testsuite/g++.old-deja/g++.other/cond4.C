// Build don't link:
// Origin: Loring Holden <lsh@cs.brown.edu>

template <class V>
class _vec3d
{
   public:
      double _x, _y;
};

class Wvec : public _vec3d<int> { };

template <class T>
class TDI {
   public:
      T   &get();
};

template <class T>
class hashvar {
  public :
   T       _val;
   TDI<T> *val() const;
   T       get() const { return true ? val()->get() : _val; }
};

int
main() {
   hashvar<Wvec>  CONSTRAINT_DIR;
   CONSTRAINT_DIR.get();
}
