// { dg-do assemble  }
// GROUPS passed constructors
class test1 {
};

template<class T>
class GC_PTR {
public:
  GC_PTR(T &a) {}
};


void
gotPtrs(GC_PTR<test1> r1)
{
}

static void
short_alloc(int n)
{
        test1 here;
        GC_PTR<test1> foo = here;   // This works fine.

        gotPtrs(here);              // Compile error from this
        // No constructor named `GC_PTR` in visible scope
        // conversion between incompatible aggregate types requested
}
