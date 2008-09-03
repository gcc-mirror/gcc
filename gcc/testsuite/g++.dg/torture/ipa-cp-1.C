/* { dg-do compile } */

// With IPA-CP, this caused a problem on darwin, where
// _M_reset is being cloned, it was still being marked
// as weak and then we had to change the calls to the
// newly marked function for the non throwing behavior.

int&  f(int&);
inline void _M_reset(int &_M_vbp) throw()
{
  f(_M_vbp);
}
extern int _S_last_request;
void _M_allocate_single_object() throw()
{
  _M_reset(_S_last_request);
  _M_reset(_S_last_request);
}
