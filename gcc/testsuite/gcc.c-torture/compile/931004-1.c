#define A "This is a long test that tests the structure initialization"
#define B A,A
#define C B,B,B,B
#define D C,C,C,C
int main()
{
  char *subs[]={ D, D, D, D, D, D, D, D, D, D, D, D, D, D, D};
}
