/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "requires hosted libstdc++ for bitset" { ! hostedlib } } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64d -O2 -std=c++03" } */

/* Model the std::bitset predicate used by 541.leela_r's
   Playout::passthrough and its inlined branch in UCTNode::updateRAVE.  */

#include <bitset>

class FastBoard
{
public:
  static const int MAXBOARDSIZE = 19;
  static const int MAXSQ = ((MAXBOARDSIZE + 2) * (MAXBOARDSIZE + 2));
  static const int PASS = -1;
  static const int BLACK = 0;
};

class Playout
{
public:
  typedef std::bitset<FastBoard::MAXSQ> bitboard_t;

  bool
  passthrough (int color, int vertex)
  {
    if (vertex == FastBoard::PASS)
      return false;

    return m_sq[color][vertex];
  }

private:
  bitboard_t m_sq[2];
};

extern void sink ();

void
update_rave (Playout &playout, int move)
{
  bool bpass = playout.passthrough (FastBoard::BLACK, move);

  if (bpass)
    sink ();
}

/* { dg-final { scan-assembler-times {\mbext\t} 1 } } */
