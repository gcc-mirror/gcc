// Special g++ Options: -fexceptions -O9

int promote_mode (int mode, int *punsignedp)
{
  int unsignedp = *punsignedp;
  *punsignedp = unsignedp;
  return mode;
}

int main() {
  int i;
  i = promote_mode (42, &i);
  return i != 42;
}
