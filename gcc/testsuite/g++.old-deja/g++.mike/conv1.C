enum E { C };

E foo() {
  return C;
}

int main() {
  if (foo() != C)
    return 1;
}
