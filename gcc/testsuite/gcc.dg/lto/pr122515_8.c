typedef struct {
  int num;
  int foo[40000000];
} A_8;

A_8 a1_8 = {1};
A_8 a2_8 = {2};

int bar_8 (int i)
{
  return i++;
}
