/* { dg-do compile } */
/* { dg-options "-fgimple -fdump-tree-sccopy -O2" } */
/* { dg-final { scan-tree-dump "Replacing SCC of size 2" "sccopy1" } } */

int __GIMPLE (ssa, startwith ("sccopy"))
main ()
{
  int a;
  int y;
  int x;
  int _1;
  int _2;
  int _13;

  __BB(2):
  if (x_7(D) == 5)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  a_10 = x_7(D);
  goto __BB5;

  __BB(4):
  a_9 = y_8(D);
  goto __BB5;

  __BB(5):
  a_3 = __PHI (__BB3: a_10, __BB4: a_9);
  if (x_7(D) == y_8(D))
    goto __BB6;
  else
    goto __BB11;

  __BB(6):
  a_11 = a_3 + 1;
  goto __BB7;

  __BB(7):
  a_4 = __PHI (__BB6: a_11, __BB11: a_6);
label1:
  if (x_7(D) != y_8(D))
    goto __BB8;
  else
    goto __BB10;

  __BB(8):
  goto __BB9;

  __BB(9):
  a_12 = __PHI (__BB8: a_4, __BB10: a_5);
  goto __BB10;

  __BB(10,loop_header(1)):
  a_5 = __PHI (__BB7: a_4, __BB9: a_12);
label2:
  _1 = y_8(D) * 2;
  if (x_7(D) == _1)
    goto __BB9;
  else
    goto __BB11;

  __BB(11):
  a_6 = __PHI (__BB5: a_3, __BB10: a_5);
  _2 = x_7(D) * 3;
  if (y_8(D) == _2)
    goto __BB7;
  else
    goto __BB12;

  __BB(12):
  _13 = 0;
  return _13;

}


