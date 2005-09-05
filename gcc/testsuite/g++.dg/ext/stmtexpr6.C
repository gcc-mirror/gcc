// { dg-do run }
// { dg-options "" }

int a[128];

int main() {
  // Check that array-to-pointer conversion occurs in a
  // statement-expression.
  if (sizeof (({ a; })) != sizeof (int *))
    return 1;
}
