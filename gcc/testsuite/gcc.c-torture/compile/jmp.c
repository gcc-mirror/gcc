int
foo (int a)
{
  if (a)
    goto a1;
  goto a2;
 a1: goto a3;
 a2: goto a4;
 a3: goto a5;
 a4: goto a6;
 a5: goto a7;
 a6: goto a8;
 a7: goto a9;
 a8: goto a10;
 a9: goto a11;
 a10: goto a12;
 a11: goto a13;
 a12:;
 a13:;
  return -a;
}
