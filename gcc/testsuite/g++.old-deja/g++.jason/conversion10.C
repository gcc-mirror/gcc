// PRMS Id: 9019
// Bug: g++ doesn't find conversion to const char *.
// Build don't link:

struct String {
  String ();
  explicit String (const char *);
  operator const char * ();
};

int main(int argc, char **argv) 
{
	String deflt("no args");
	String useme;

	const char *p = (argv[1]) ? argv[1] : deflt;

	return 0;
}
