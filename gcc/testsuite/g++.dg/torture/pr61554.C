// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

struct A
{
  A ();
};
const unsigned long &min (const unsigned long &, const unsigned long &) {}

template <typename _InputIterator1, typename _InputIterator2,
          typename _OutputIterator, typename _BinaryOperation>
void transform (_InputIterator1 p1, _InputIterator2, _OutputIterator,
                _BinaryOperation p4)
{
  for (; p1;)
    p4 (0, 0);
}

class multi_array
{
public:
  multi_array (int &, int &);
  int &resize_ranges;
  int resize___trans_tmp_1;
  void m_fn1 ()
  {
    multi_array a (resize_ranges, this->m_fn2 ());
    const unsigned long &(*b)(const unsigned long &, const unsigned long &)
      = min;
    transform (&resize___trans_tmp_1, 0, 0, b);
    A c;
  }
  ~multi_array ()
  {
    for (int i; &base_;)
      ;
  }
  int base_;
  int &m_fn2 ();
};

class B
{
  void m_fn3 (const int &, const int &);
  multi_array _bookingSnapshotBlock;
};
void B::m_fn3 (const int &, const int &) { _bookingSnapshotBlock.m_fn1 (); }
