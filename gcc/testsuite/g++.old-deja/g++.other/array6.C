// { dg-do run  }
// { dg-options "-O1" }

int count = 0;

double foo () {
  count++;
  return 0;
}

double bar () {
  const double x[1] = { foo() };
  return x[0];
}

int main () 
{
  bar();
  if (count != 1)
    return 1;
}
