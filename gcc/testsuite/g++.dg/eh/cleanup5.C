// PR 17907
// { dg-do compile }
// { dg-options "" }
// { dg-require-effective-target alloca }
// We lost a CLEANUP_POINT_EXPR, leading to a crash destroying temp of A.


struct String {
  ~String();
  int size() const;
};
struct CodingSystem {
  String convertOut() const;
};
void inputOpened(CodingSystem *outputCodingSystem)
{
   char filePath[outputCodingSystem->convertOut().size()];
}
