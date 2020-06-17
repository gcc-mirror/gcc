// PR debug/59510
// { dg-do compile }
// { dg-options "-O2 -g --param=large-stack-frame-growth=1 -Wno-return-type" }

template <typename _Iterator>
struct _Iter_base
{
  typedef _Iterator iterator_type;
};
template <typename _CharT>
struct basic_ostream;
template <typename _CharT>
struct basic_ostringstream;
template <typename _CharT>
struct ostreambuf_iterator;
typedef basic_ostringstream <char>ostringstream;
template <typename _Iterator> struct _Miter_base : _Iter_base <_Iterator>
{
};
template <typename _Iterator>
typename _Miter_base <_Iterator>::iterator_type __miter_base (_Iterator);
template <typename _CharT>
ostreambuf_iterator <_CharT>
__copy_move_a2 (ostreambuf_iterator <_CharT>);
template <typename _II, typename _OI>
_OI copy (_II __first, _II __last, _OI __result)
{
  __copy_move_a2 <false> (__first, __miter_base (__last), __result);
}
struct ios_base {
  struct _Words {
    int *_M_pword;
    long _M_iword;
  };
  _Words _M_local_word[8];
};
template <typename _CharT>
struct basic_streambuf
{
  typedef _CharT char_type;
  int sputn (char_type *, int);
};
template <typename _CharT>
struct ostreambuf_iterator
{
  typedef basic_streambuf <_CharT> streambuf_type;
  typedef basic_ostream <_CharT> ostream_type;
  streambuf_type *_M_sbuf;
  bool _M_failed;
  ostreambuf_iterator (ostream_type __s) : _M_sbuf (__s.rdbuf ()), _M_failed () {}
  void _M_put (_CharT * __ws, int __len)
  {
    if (_M_failed && _M_sbuf->sputn (__ws, __len) != __len) _M_failed = true;
  }
};
template <bool, typename _CharT>
void __copy_move_a2 (_CharT * __first,_CharT * __last,ostreambuf_iterator <_CharT> __result)
{
  int __num = __last - __first;
  __result._M_put (__first, __num);
}
template <typename _CharT>
struct basic_ios : ios_base
{
  basic_streambuf <_CharT> *rdbuf ();
};
template <typename _CharT>
struct basic_ostream : public basic_ios <_CharT>
{
};
template <typename _CharT>
struct basic_ostringstream : public basic_ostream <_CharT>
{
};
void
test01 () {
  char data1[] = "foo";
  char *beg1 = data1;
  ostringstream oss1;
  ostreambuf_iterator <char> out1 (oss1);
  out1 = copy (beg1, beg1, out1);
}
