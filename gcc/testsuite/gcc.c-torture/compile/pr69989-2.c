int a, b, d;
short c[] = {4073709551611, 1, 4, 4};

void fn1() {
  if (a)
    goto LABEL_vhvhP;
  for (;;) {
    for (; b; b++)
      d = c[b + 3] | c[b];
  LABEL_vhvhP:
    if (d)
      break;
  }
}

int main() { return 0; }
