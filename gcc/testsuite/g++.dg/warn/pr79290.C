// { dg-additional-options "-Wall" }
// PR 79290, bogus warning looking inside PMF

struct Song {
  int get() const ;
};

typedef int (Song::*PMF_t)() const;

struct SongTag {
  PMF_t function () const;
};


template<typename T>
struct Printer {
  bool Foo(const SongTag &st) {
    return st.function () == &Song::get;
  }
};

void Baz (Printer<int> *p, SongTag const &st)
{
  p->Foo (st);
}
