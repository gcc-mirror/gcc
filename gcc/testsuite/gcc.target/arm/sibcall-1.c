/* { dg-do compile { target { arm32 } } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "FDPIC does not support sibcall optimization" { arm*-*-uclinuxfdpiceabi } "*" "" } */

#define noinline __attribute__((noinline))

typedef struct {
  int data[4];
} arr16_t;

int result = 0;

void noinline func2 (int i, int j, arr16_t arr)
{
  result = (arr.data[0] != 1
	    || arr.data[1] != 2
	    || arr.data[2] != 3
	    || arr.data[3] != 4);
}

void func1 (int i, int j, int k, int l, int m, int n, arr16_t a)
{
  func2(i, j, a);
}

int main(int argc, const char *argv[])
{
  arr16_t arr = {{1, 2, 3, 4}};
    
  func1(0, 0, 0, 0, 0, 0, arr);
  return result;
}

/* The PLT marker may appear if the test is run with -fpic/-fPIC.  */
/* { dg-final { scan-assembler "\tb\tfunc2(\\(PLT\\))?\n" } } */

