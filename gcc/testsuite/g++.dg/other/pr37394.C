// Origin: Martin Michlmayr <tbm@cyrius.com>
// { dg-do compile { target ia64-*-* } }
// { dg-options "-O -fschedule-insns2" }

struct _Words
{
  void *_M_pword;
  _Words ():
   _M_pword (0)
  {
  }
} _M_word_zero;
