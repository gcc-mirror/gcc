typedef struct {
  int num;
  int foo[40000000];
} A_4;

A_4 a1_4 = {1};
A_4 a2_4 = {2};

int bar_4 (int i)
{
  return i++;
}
