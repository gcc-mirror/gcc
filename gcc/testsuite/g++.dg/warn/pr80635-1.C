// PR tree-optimization/80635
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Wmaybe-uninitialized" }

using size_t = decltype (sizeof (1));
inline void *operator new (size_t, void *p) { return p; }
template<typename T>
struct optional
{
  optional () : m_dummy (), live (false) {}
  void emplace () { new (&m_item) T (); live = true; }
  ~optional () { if (live) m_item.~T (); }

  union
  {
    struct {} m_dummy;
    T m_item;
  };
  bool live;
};

extern int get ();
extern void set (int);

struct A
{
  A () : m (get ()) {}
  ~A () { set (m); }	// { dg-bogus "may be used uninitialized in this function" }

  int m;
};

struct B
{
  B ();
  ~B ();
};

void func ()
{
  optional<A> maybe_a;
  optional<B> maybe_b;

  maybe_a.emplace ();
  maybe_b.emplace ();
}
