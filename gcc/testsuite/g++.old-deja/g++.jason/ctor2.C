// { dg-do assemble  }
// Bug: the reference to c in the initializer list doesn't get fixed up.

struct AP {
    AP(unsigned char);
};

struct AI : AP {
    AI(unsigned char);
};

AI::AI(unsigned char c)
: AP(c)
{
  &c;
}
