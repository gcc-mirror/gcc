/* PR middle-end/98082 */
/* Reported by Martin Liska <marxin@gcc.gnu.org> */

/* { dg-do compile { target c++20 } } */
/* { dg-options "-fipa-icf" } */

class GoodIter {
public:
  GoodIter();
  GoodIter(GoodIter &);
};

GoodIter operator-(int, GoodIter) { return GoodIter(); }
GoodIter operator+(int, GoodIter) { return GoodIter(); }
