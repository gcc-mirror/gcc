/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -ftrapv -fexceptions" } */
/* { dg-require-effective-target exceptions } */
void
DrawChunk(int *tabSize, int x) 
{
  const int numEnds = 10;
  int ends[numEnds + 2];
  if (*tabSize > 0) {
      x -= 5;
      x = (x + *tabSize) / *tabSize;
  }
}

