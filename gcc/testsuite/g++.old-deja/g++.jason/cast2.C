// Build don't link:
// The compiler tried to build up a double with a NOP_EXPR from
// integer_zero_node, which fails.

template <class T>
class vector  {
public:
    vector (int n, const T& value = T()) {}
};

void
foo (void)
{
  vector<double> v (10);
}
