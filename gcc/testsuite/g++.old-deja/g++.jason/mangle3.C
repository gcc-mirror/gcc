// { dg-do run  }
// PRMS Id: 7563
// Bug: declaration at function scope causes mismangling.

int main()
{
  char ArrA[1][1][2][3],
       ArrB[1][1][2][3];

  void HitMe(char [1][1][2][3], char [1][1][2][3]);

  HitMe(ArrA, ArrB);

  return 0;
}

void HitMe(char A[1][1][2][3], char B[1][1][2][3])
{
  return;
}
