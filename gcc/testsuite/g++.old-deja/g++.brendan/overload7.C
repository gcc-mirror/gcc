// GROUPS passed overloading
extern "C" int printf (const char *, ...);

struct NoName {
        
        int first;
        int second;
};

class Casted {

  public:

        NoName  x;
        double  y;

        Casted ( int _x , double _y ): y(_y) 
        { 
                x.first = _x;
                x.second = _x*2;
        }
        
        operator NoName() const { return x; }
        operator double() const { return y; }
};

int main()
{
        Casted c(10,12.34);

        NoName x;
        double y;

        x = c;
        y = c;

	if (x.first == 10 && x.second == 20 && y == 12.34)
	  printf ("PASS\n");
	else
	  printf ("FAIL\n");
}
