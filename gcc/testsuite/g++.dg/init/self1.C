// PR c++/29106
// { dg-do run } 

int i;

void f(__SIZE_TYPE__) {
  i = 3;
}


int main()
{
  int* const savepos = sizeof(*savepos) ? 0 : 0;

  f (sizeof (*savepos));

  if (i != 3)
    return 1;
}
