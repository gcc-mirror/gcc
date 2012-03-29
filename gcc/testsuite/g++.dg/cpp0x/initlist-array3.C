// PR c++/52743
// { dg-do compile { target c++11 } }

void composite (int const (&) [2]);
void composite (int const (&) [3]);

int main ()
{
  composite({0,1});		// { dg-error "ambiguous" }
}
