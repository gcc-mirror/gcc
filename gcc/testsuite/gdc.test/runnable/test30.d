// 444

int main()
{
  int nothing( int delegate(ref int) dg ) {return 0;}
  foreach(int x; &nothing)
    return 7;
  return 0;
}

