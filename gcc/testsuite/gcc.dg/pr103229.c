// { dg-do compile }
// { dg-options "-O -w" }

int main() {
  int i;
  for (; i;)
    ;

  return 0;
}
