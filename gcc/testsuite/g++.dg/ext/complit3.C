// { dg-options "" }

int Compound_Literals_0()
{
  static int y[] = (int []) {1, 2, 3}; // { dg-error "" }
  static int z[] = (int [3]) {1}; // { dg-error "" }
  return y[0]+z[0]; 
}
