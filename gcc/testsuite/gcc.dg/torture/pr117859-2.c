/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */

/* PR tree-optimization/117859 */

void __GIMPLE (ssa,startwith("pre"))
m (int p, int b)
{
  int _6;
  _Bool _48;
  _Bool _50;
  int _57;
  _Bool _59;

  __BB(2):
  _6 = 0; // This needs to be prop'ed to make _48 unconditional
  if (_6 != 0)
    goto __BB5;
  else
    goto __BB14;

  __BB(14):
  _48 = p_2(D) != 0;
  goto __BB7;

  __BB(5):
  goto __BB7;

  __BB(7):
  _59 = __PHI (__BB5: _Literal (_Bool) 1, __BB14: _48);
  if (b_36(D) != 0)
    goto __BB8;
  else
    goto __BB13;

  __BB(8):
  _50 = _59 != _Literal (_Bool) 0; // Important being != 0
  _57 = _50 ? 1 : 0; // can be unused but need around for _50 being used
  goto __BB9;

  __BB(9,loop_header(2)):
  if (_59 != _Literal (_Bool) 0)
    goto __BB13;
  else
    goto __BB9;

  __BB(13):
  return;


}
