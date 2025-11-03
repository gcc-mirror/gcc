typedef struct {
  int num;
  int foo[40000000];
} A_1;

A_1 a1_1 = {1};
A_1 a2_1 = {2};

int bar_1 (int i)
{
  return i++;
}
