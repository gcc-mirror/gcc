// PRMS Id: 5720
// Bug: the extra set of parens confuses the expr/declarator disambiguation.

class Fu
{
  int val;
public:
  Fu(int i) : val(i) { };
  void print() { }
};

int main(int argc, char * argv[])
{
  int * i = &argc;

  Fu((*i)).print();		// gets bogus error
  Fu((*j));
}
