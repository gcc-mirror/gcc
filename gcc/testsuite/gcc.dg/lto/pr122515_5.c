typedef struct {
  int num;
  int foo[40000000];
} A_5;

A_5 a1_5 = {1};
A_5 a2_5 = {2};

int bar_5 (int i)
{
  return i++;
}
