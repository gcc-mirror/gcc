typedef struct {
  int num;
  int foo[40000000];
} A_9;

A_9 a1_9 = {1};
A_9 a2_9 = {2};

int bar_9 (int i)
{
  return i++;
}
