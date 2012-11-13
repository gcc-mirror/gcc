int array[2];

static int
func1 (int b)
{
  return b;
}

static int
func2 (int a, int b)
{
  return b == 0 ? a : b;
}

int
func3 (int a)
{
}

int *
func4 (int *arg)
{
  *arg = func1 ((func2 (func3 (array[0]), *arg)) | array[0]);
  return &array[1];
}
