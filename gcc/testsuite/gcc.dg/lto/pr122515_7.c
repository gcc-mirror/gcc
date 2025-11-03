typedef struct {
  int num;
  int foo[40000000];
} A_7;

A_7 a1_7 = {1};
A_7 a2_7 = {2};

int bar_7 (int i)
{
  return i++;
}
