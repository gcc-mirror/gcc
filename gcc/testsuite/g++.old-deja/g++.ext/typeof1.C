// Build don't link:

// crash test - XFAIL *-*-*

struct inttest {
  int elem[1];
};

template <class T>
void F(T x)
{
  typedef __typeof__(x.elem[0]) dummy;
  i = 1;
}

int main() {
  inttest x;
  F(x);
}
