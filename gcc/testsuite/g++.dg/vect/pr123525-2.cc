/* { dg-do compile } */
/* { dg-additional-options "-march=tigerlake -std=c++23" { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-std=c++23" } */

short ScAddress_nTabP;
struct ScAddress {
  short nCol;
  short nTab;
  ScAddress(short nColP) : nCol(nColP), nTab(ScAddress_nTabP) {}
};

struct ScRange {
  ScAddress aStart;
};

struct CellRangeAddress {
  int StartColumn;
  int StartRow;
  int EndColumn;
  int EndRow;
};

struct ScCellRangeObj {
  ScCellRangeObj(int, ScRange &);
};

CellRangeAddress ScVbaRangeMergeArea_aCellAddress;
void ScVbaRangeMergeArea() {
  if (ScVbaRangeMergeArea_aCellAddress.StartColumn == 0 &&
      ScVbaRangeMergeArea_aCellAddress.EndColumn == 0 &&
      ScVbaRangeMergeArea_aCellAddress.StartRow == 0 &&
      ScVbaRangeMergeArea_aCellAddress.EndRow == 0)
    ;
  else {
    ScRange refRange(ScVbaRangeMergeArea_aCellAddress.StartColumn);
    ScCellRangeObj(0, refRange);
  }
}
