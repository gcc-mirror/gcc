// Build don't link:

struct inttest {
  int elem[1];
};

template <class T>
void F(T x)
{
  typedef __typeof (x.elem[0]) dummy;
  dummy i = 1;
}

int main() {
  inttest x;
  F(x);
}
