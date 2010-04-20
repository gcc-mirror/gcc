// { dg-do run }

#include <vector>

std::vector <int>
sequence(int l, int n)
{
  std::vector <int> ret;
  for(int i=n;i<=100;i++)
    {
      if(i%2==0)
	{
	  if(l%i==i/2)
	    {
	      int init =l/i-i/2+1;
	      if(init>=0)
		{
		  for(int j=0;j<i;j++)
		    {
		      ret.push_back(init);
		      init ++;
		    }
		  break;
		}
	    }
	}
      else
	{
	  if(l%i==0)
	    {
	      int init =l/i-i/2;
	      if(init>=0)
		{
		  for(int j=0;j<i;j++)
		    {
		      ret.push_back(init);
		      init ++;
		    }
		  break;
		}
	    }
	}
    }
  return ret;
}
extern "C" void abort (void);
int main()
{
  std::vector<int> res = sequence(18, 2);
  if (res.size () != 3
      || res[0] != 5
      || res[1] != 6
      || res[2] != 7)
    abort ();
  return 0;
}
