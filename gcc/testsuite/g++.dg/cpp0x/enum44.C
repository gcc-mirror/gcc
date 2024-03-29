// PR c++/103825
// { dg-do compile { target c++11 } }

enum class Type { Pawn };
struct Piece {
  Type type : 4;
};

void
foo ()
{
  switch (Piece().type)
    case Type::Pawn:;

  auto x = Piece().type;
  switch (x)
    case Type::Pawn:;
}

enum class En {A};
struct St {En field :1;};

void
bar ()
{
  volatile St s = {En::A};
  switch(s.field) {
    case En::A : break;
  }
}
