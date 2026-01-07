typedef struct {
  int num;
  int foo[40000000];
} A_3;

A_3 a1_3 = {1};
A_3 a2_3 = {2};

int bar_3 (int i)
{
  return i++;
}
