typedef long int Int;
void FuncMakeConsequencesPres (long *objDefs1)
{
  long a = (long)objDefs1;
  int c = a & 0x01;
  int b = 0;
  if (!  ( 13 <= ( c ? 0 : (((int) objDefs1 & 0x02) ? 0 : *objDefs1  ))
           && b <= 0))
    ErrorQuit ();
}

