/* { dg-do run } */
/* { dg-options "-O2 -foptimize-sibling-calls" } */

typedef struct {
  int data[4];
} arr16_t;

int result = 0;

void func2(int i, int j, arr16_t arr)
{
  result = (arr.data[0] != 1
	    || arr.data[1] != 2
	    || arr.data[2] != 3
	    || arr.data[3] != 4);
}

void func1(int i, int j, int k, arr16_t a)
{
  func2(i, j, a);
}

int main(int argc, const char *argv[])
{
  arr16_t arr = {{1, 2, 3, 4}};
    
  func1(0, 0, 0, arr);
  return result;
}

