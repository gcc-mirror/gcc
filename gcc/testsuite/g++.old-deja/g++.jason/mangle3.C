// PRMS Id: 7563
// Bug: declaration at function scope causes mismangling.

int main()
{
  char ArrA[10][10][20][30],
       ArrB[10][10][20][30];

  void HitMe(char [10][10][20][30], char [10][10][20][30]);

  HitMe(ArrA, ArrB);

  return 0;
}

void HitMe(char A[10][10][20][30], char B[10][10][20][30])
{
  return;
}
