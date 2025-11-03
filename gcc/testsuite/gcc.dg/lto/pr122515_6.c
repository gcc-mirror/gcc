typedef struct {
  int num;
  int foo[40000000];
} A_6;

A_6 a1_6 = {1};
A_6 a2_6 = {2};

int bar_6 (int i)
{
  return i++;
}
