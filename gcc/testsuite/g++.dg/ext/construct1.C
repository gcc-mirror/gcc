// PR c++/16717
// { dg-options "-O2" }

int i;

void hello (void) __attribute__ ((constructor));
void hello (void) { i = 1; }

int main (void) {
  if (i != 1)
    return 1;
}
