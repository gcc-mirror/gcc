// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options { "-O3 -flto -fPIC" } }

void WriteLiteral( unsigned long data, unsigned long bits) {}
void WriteQIndexDelta( short qDelta)
{
  WriteLiteral(__builtin_abs(qDelta), 4);
}
__attribute((used))
void ff(signed char *qIndexDeltaLumaDC) {
  WriteQIndexDelta(*qIndexDeltaLumaDC);
}
int main(){}
