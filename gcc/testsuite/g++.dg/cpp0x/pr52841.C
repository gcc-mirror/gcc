// { dg-do compile }

struct Solvable;
namespace sat
{
  class Solvable
    {
  public:
      typedef bool bool_type;
    };
}

class Resolvable : public sat::Solvable
{
public:
  using sat::Solvable::bool_type;
};
