// { dg-do assemble  }
// Origin: Corey Kosak <kosak@cs.cmu.edu>

struct cow_t {
  template<bool Q>
  static void tfunc(cow_t *cowp) {}

  void moo() {
    cow_t *cowp;
    tfunc<true>(cowp);
  }
};


int main()
{
  cow_t *cowp;
  cow_t::tfunc<true>(cowp);
}
