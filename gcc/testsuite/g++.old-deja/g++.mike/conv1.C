enum E { C };

E foo() {
  return C;
}

main() {
  if (foo() != C)
    return 1;
}
