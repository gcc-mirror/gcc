// PR c++/29106

int i;

void f(__SIZE_TYPE__) {
  i = 3;
}


int main()
{
  int* const savepos = sizeof(*savepos) ? 0 : 0;  /* { dg-error "invalid conversion" "convert" { target c++11 }  } */

  f (sizeof (*savepos));

  if (i != 3)
    return 1;
}
