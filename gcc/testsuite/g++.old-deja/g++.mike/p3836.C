// { dg-do assemble  }
// prms-id: 3836

void f(int &i) {	// { dg-error "" } ref line
  i = 10;
}

int main()
{
  int i=1, j=2;
  f(i);
  f((int)j);		// { dg-error "" } passing in to non-const
  if (j != 2)
    return 1;
}
