/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -ffast-math -fsignaling-nans" } */
/* { dg-warning "-fassociative-math disabled" "" { target *-*-* } 1 } */

template <class T>
class ggStaticArray {
public:
  ~ggStaticArray();
};

template <class T>
class ggGrid {
public:
  ggGrid() : grid() { }
  ggStaticArray<T> grid;
};

class mrGrid {
public:
  mrGrid(void);
protected:
  ggGrid<int*> grid;
  double multiplier;
};

mrGrid::mrGrid(void)
{
  double xMeasure, yMeasure, zMeasure;
  double cellDimension;

  cellDimension = multiplier * (xMeasure * yMeasure * zMeasure);
}
