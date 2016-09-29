int i;

void hello (void) __attribute__ ((constructor));
void hello (void) { i = 1; }

int main (void) {
  if (i != 1)
    return 1;

  return 0;
}
