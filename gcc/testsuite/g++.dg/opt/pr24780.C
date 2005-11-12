// PR c++/24780
// { dg-do compile }

template<typename S=int>
struct Move {
  Move();
  static Move<S> const MOVES[2][2];
};
template<typename S>
  Move<S> const Move<S>::MOVES[2][2]={};
typedef Move<int> const MoveClass;
void moves(int x, int y) {
  &MoveClass::MOVES[x][y];
}
