// Bug: the reference to c in the initializer list doesn't get fixed up.
// Build don't link:

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
