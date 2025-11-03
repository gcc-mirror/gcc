typedef struct {
  int num;
  int foo[40000000];
} A_2;

A_2 a1_2 = {1};
A_2 a2_2 = {2};

int bar_2 (int i)
{
  return i++;
}
