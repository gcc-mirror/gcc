extern short distdata[64][64], taxidata[64][64];
extern short PieceList[2][64];

int
ScoreKBNK (short int winner, short int king1, short int king2)
{
  register short s;

  s = taxidata[king1][king2] + distdata[PieceList[winner][1]][king2];
  return s;
}
