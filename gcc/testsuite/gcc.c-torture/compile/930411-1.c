int heap;

g(){}

f(int i1, int i2)
{
  i1 = *(int*)(i1 + 4);
  if (i1 == 0)
    goto L4;
  else
    goto L9;
 L3:
  i2 = heap - 8;
  *(int*)i2 = 3;
  *(int*)(i2 + 4) = i1;
  heap -= 8;
  return i2;
 L4:
  i1 = g(i2);
  goto L5;
 L5:
  i1 = *(int*)(i1 + 4);
  if (i1 == 0)
    goto L7;
  else
    goto L8;
 L7:
  i1 = 0;
  goto L3;
 L8:
  i1 = 1;
  goto L3;
 L9:
  i1 = 1;
  goto L3;
}
