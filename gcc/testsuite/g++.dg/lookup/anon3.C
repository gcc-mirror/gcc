// Test that anonymous unions work with explicit scope.

static union
{
  int i;
};

int main()
{
  return ::i;
}
