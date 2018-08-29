// PR c++/84489
// { dg-do compile { target c++11 } }

template <class T = int, T N = T(), bool B = (N >> 1)>
T f1() {return 0;}

int main()
{
  f1(); // Bug here
}
