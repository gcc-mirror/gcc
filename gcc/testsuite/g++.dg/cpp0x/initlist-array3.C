// PR c++/52743
// { dg-do compile { target c++11 } }

void composite (int const (&) [2]);
void composite (int const (&) [3]);

int main ()
{
  // Not ambiguous since CWG 1307.
  composite({0,1});
}
