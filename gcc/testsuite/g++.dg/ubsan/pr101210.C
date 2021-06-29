// PR c++/101210
// { dg-do run }
// { dg-options "-fsanitize=null,alignment -fno-sanitize-recover=null,alignment" }

int v[2];
int
main ()
{
  int x;
  int &y = x;
  v[0] = reinterpret_cast<__INTPTR_TYPE__>(&y) == 0;
  v[1] = reinterpret_cast<__INTPTR_TYPE__>(&y) == 1;
}
