// PR c++/51313
// { dg-do compile { target c++11 } }

class ostream;

extern "C" {
  extern int isdigit (int);
}

ostream&
operator<<(ostream&, const unsigned char*);

extern ostream cout;

int main()
{
  cout << isdigit(0);
}
