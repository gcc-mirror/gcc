// Build don't link: 
// GROUPS passed old-abort
extern "C" void printf (char *, ...);


class cl
{
  int i;
public:
  cl(int j = 0) {i = j;}
  int get_i() {return i;}
  };

int
main()
{
  cl ob[3] = {1, 2, 3};
  int i;

  for(i=0; i<3; i++)
    printf("%d\n", ob[i].get_i());

  return 0;
  }
