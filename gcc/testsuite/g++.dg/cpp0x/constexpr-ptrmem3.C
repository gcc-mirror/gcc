// PR c++/62129
// { dg-do compile { target c++11 } }

class Evaluator
{
  int MakeChangelist ();
  typedef int (Evaluator::*fac_t)();
  struct CreatorEntry
  {
    const char *type;
    fac_t factory;
  };
  static constexpr CreatorEntry kCreators[] = { "", &Evaluator::MakeChangelist };
};

constexpr Evaluator::CreatorEntry Evaluator::kCreators[];
