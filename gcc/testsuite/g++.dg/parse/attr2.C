// PR c++/16337

#define vector __attribute__((vector_size(16)))
void foo (void)
{
  vector signed int v1 = { 1, 2, 3, 4 };
  vector signed int v2, v3;
  vector signed int v4;
  v2 = v1;
  v3 = v1;
  v4 = v1;
}
