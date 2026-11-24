// PR tree-optimization/112628
// { dg-do run }
// { dg-require-effective-target int32 }
// { dg-options "-fsanitize=signed-integer-overflow" }

__attribute__((noipa)) int
f (int a)
{
	return (~a) < -a;
}

int main()
{
  volatile int x = -__INT_MAX__ - 1;
  f (x);
  return 0;
}

// { dg-output "negation of -2147483648 cannot be represented in type 'int'" }
