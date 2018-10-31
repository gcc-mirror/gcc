void test4090a()
{
    // for the mutable elements
    {
        int[] arr = [1,2,3];

        // inference + qualifier
        foreach (          x; arr) static assert(is(typeof(x) == int));
        foreach (    const x; arr) static assert(is(typeof(x) == const int));
        foreach (immutable x; arr) static assert(is(typeof(x) == immutable int));

        // inference + qualifier + ref
        foreach (          ref x; arr) static assert(is(typeof(x) == int));
        foreach (    const ref x; arr) static assert(is(typeof(x) == const int));
      static assert(!__traits(compiles, {
        foreach (immutable ref x; arr) {}
      }));

        // with exact type + qualifier
        foreach (          int x; arr) static assert(is(typeof(x) == int));
        foreach (    const int x; arr) static assert(is(typeof(x) == const int));
        foreach (immutable int x; arr) static assert(is(typeof(x) == immutable int));

        // with exact type + qualifier + ref
        foreach (          ref int x; arr) static assert(is(typeof(x) == int));
        foreach (    const ref int x; arr) static assert(is(typeof(x) == const int));
      static assert(!__traits(compiles, {
        foreach (immutable ref int x; arr) {}
      }));

        // convertible type + qualifier
        foreach (          double x; arr) static assert(is(typeof(x) == double));
        foreach (    const double x; arr) static assert(is(typeof(x) == const double));
        foreach (immutable double x; arr) static assert(is(typeof(x) == immutable double));

        // convertible type + qualifier + ref
      static assert(!__traits(compiles, {
        foreach (          ref double x; arr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (    const ref double x; arr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (immutable ref double x; arr) {}
      }));
    }
    // for the immutable elements
    {
        immutable(int)[] iarr = [1,2,3];

        // inference + qualifier
        foreach (          x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (    const x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (immutable x; iarr) static assert(is(typeof(x) == immutable int));

        // inference + qualifier + ref
        foreach (          ref x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (    const ref x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (immutable ref x; iarr) static assert(is(typeof(x) == immutable int));

        // with exact type + qualifier
        foreach (          int x; iarr) static assert(is(typeof(x) == int));
        foreach (    const int x; iarr) static assert(is(typeof(x) == const int));
        foreach (immutable int x; iarr) static assert(is(typeof(x) == immutable int));

        // with exact type + qualifier + ref
      static assert(!__traits(compiles, {
        foreach (          ref int x; iarr) {}
      }));
        foreach (    const ref int x; iarr) static assert(is(typeof(x) == const int));
        foreach (immutable ref int x; iarr) static assert(is(typeof(x) == immutable int));

        // convertible type + qualifier
        foreach (          double x; iarr) static assert(is(typeof(x) == double));
        foreach (    const double x; iarr) static assert(is(typeof(x) == const double));
        foreach (immutable double x; iarr) static assert(is(typeof(x) == immutable double));

        // convertible type + qualifier + ref
      static assert(!__traits(compiles, {
        foreach (ref double x; iarr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (const ref double x; iarr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (immutable ref double x; iarr) {}
      }));
    }
}

void test4090b()
{
    // for the key
    {
        int[] arr = [1,2,3];

        // inference + qualifier
        foreach (          i, x; arr) static assert(is(typeof(i) == size_t));
        foreach (    const i, x; arr) static assert(is(typeof(i) == const size_t));
        foreach (immutable i, x; arr) static assert(is(typeof(i) == immutable size_t));

        // inference + qualifier + ref
        foreach (          ref i, x; arr) static assert(is(typeof(i) == size_t));
        foreach (    const ref i, x; arr) static assert(is(typeof(i) == const size_t));
      static assert(!__traits(compiles, {
        foreach (immutable ref i, x; arr) {}
      }));

        // with exact type + qualifier
        foreach (          size_t i, x; arr) static assert(is(typeof(i) == size_t));
        foreach (    const size_t i, x; arr) static assert(is(typeof(i) == const size_t));
        foreach (immutable size_t i, x; arr) static assert(is(typeof(i) == immutable size_t));

        // with exact type + qualifier + ref
        foreach (          ref size_t i, x; arr) static assert(is(typeof(i) == size_t));
        foreach (    const ref size_t i, x; arr) static assert(is(typeof(i) == const size_t));
      static assert(!__traits(compiles, {
        foreach (immutable ref size_t i, x; arr) {}
      }));
    }

    // for the mutable elements
    {
        int[] arr = [1,2,3];

        // inference + qualifier
        foreach (i,           x; arr) static assert(is(typeof(x) == int));
        foreach (i,     const x; arr) static assert(is(typeof(x) == const int));
        foreach (i, immutable x; arr) static assert(is(typeof(x) == immutable int));

        // inference + qualifier + ref
        foreach (i,           ref x; arr) static assert(is(typeof(x) == int));
        foreach (i,     const ref x; arr) static assert(is(typeof(x) == const int));
      static assert(!__traits(compiles, {
        foreach (i, immutable ref x; arr) {}
      }));

        // with exact type + qualifier
        foreach (i,           int x; arr) static assert(is(typeof(x) == int));
        foreach (i,     const int x; arr) static assert(is(typeof(x) == const int));
        foreach (i, immutable int x; arr) static assert(is(typeof(x) == immutable int));

        // with exact type + qualifier + ref
        foreach (i,           ref int x; arr) static assert(is(typeof(x) == int));
        foreach (i,     const ref int x; arr) static assert(is(typeof(x) == const int));
      static assert(!__traits(compiles, {
        foreach (i, immutable ref int x; arr) {}
      }));

        // convertible type + qualifier
        foreach (i,           double x; arr) static assert(is(typeof(x) == double));
        foreach (i,     const double x; arr) static assert(is(typeof(x) == const double));
        foreach (i, immutable double x; arr) static assert(is(typeof(x) == immutable double));

        // convertible type + qualifier + ref
      static assert(!__traits(compiles, {
        foreach (i,           ref double x; arr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (i,     const ref double x; arr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (i, immutable ref double x; arr) {}
      }));
    }
    // for the immutable elements
    {
        immutable(int)[] iarr = [1,2,3];

        // inference + qualifier
        foreach (i,           x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (i,     const x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (i, immutable x; iarr) static assert(is(typeof(x) == immutable int));

        // inference + qualifier + ref
        foreach (i,           ref x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (i,     const ref x; iarr) static assert(is(typeof(x) == immutable int));  // same as variable declaration
        foreach (i, immutable ref x; iarr) static assert(is(typeof(x) == immutable int));

        // with exact type + qualifier
        foreach (i,           int x; iarr) static assert(is(typeof(x) == int));
        foreach (i,     const int x; iarr) static assert(is(typeof(x) == const int));
        foreach (i, immutable int x; iarr) static assert(is(typeof(x) == immutable int));

        // with exact type + qualifier + ref
      static assert(!__traits(compiles, {
        foreach (i,           ref int x; iarr) {}
      }));
        foreach (i,     const ref int x; iarr) static assert(is(typeof(x) == const int));
        foreach (i, immutable ref int x; iarr) static assert(is(typeof(x) == immutable int));

        // convertible type + qualifier
        foreach (i      ,     double x; iarr) static assert(is(typeof(x) == double));
        foreach (i,     const double x; iarr) static assert(is(typeof(x) == const double));
        foreach (i, immutable double x; iarr) static assert(is(typeof(x) == immutable double));

        // convertible type + qualifier + ref
      static assert(!__traits(compiles, {
        foreach (i, ref double x; iarr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (i, const ref double x; iarr) {}
      }));
      static assert(!__traits(compiles, {
        foreach (i, immutable ref double x; iarr) {}
      }));
    }
}

void test4090c()
{
    foreach (          x; 1..11) static assert(is(typeof(x) == int));
    foreach (    const x; 1..11) static assert(is(typeof(x) == const int));
    foreach (immutable x; 1..11) static assert(is(typeof(x) == immutable int));

    foreach (          int x; 1..11) static assert(is(typeof(x) == int));
    foreach (    const int x; 1..11) static assert(is(typeof(x) == const int));
    foreach (immutable int x; 1..11) static assert(is(typeof(x) == immutable int));

    foreach (          ref x; 1..11) static assert(is(typeof(x) == int));
    foreach (    const ref x; 1..11) static assert(is(typeof(x) == const int));
  static assert(!__traits(compiles, {
    foreach (immutable ref x; 1..11) {}
  }));

    foreach (          double x; 1..11) static assert(is(typeof(x) == double));
    foreach (    const double x; 1..11) static assert(is(typeof(x) == const double));
    foreach (immutable double x; 1..11) static assert(is(typeof(x) == immutable double));

    foreach (          ref double x; 1..11) static assert(is(typeof(x) == double));
    foreach (    const ref double x; 1..11) static assert(is(typeof(x) == const double));
  static assert(!__traits(compiles, {
    foreach (immutable ref double x; 1..11) {}
  }));
}
