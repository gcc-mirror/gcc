/* PR c/98597 - ICE in -Wuninitialized printing a MEM_REF
   { dg-do compile }
   { dg-options "-O2 -Wall -fno-tree-vectorize" } */

/* After vectorization, the location of the warning that's off,
   the warning itself is still issued but it's swallowed by
   the dg-prune-output directive. Refer to pr102700.  */
struct shared_count {
  shared_count () { }
  shared_count (shared_count &r)
    : pi (r.pi) { }     // { dg-warning "\\\[-Wuninitialized" }
  int pi;
};

// There's another (redundant) -Wuninitialized on the line below.
struct shared_ptr {
  int ptr;
  shared_count refcount;
};

struct Bar {
  Bar (int, shared_ptr);
};

void g () {
  shared_ptr foo;
  Bar (0, foo);
}

// Prune out duplicates.
// { dg-prune-output "-Wuninitialized" }
