// PRMS Id: 7088
// Build don't link:

struct string
{
  int length () const;
  string (string &);
  string (char * = 0);
  int operator [] (int);
  ~string ();
};

void _cook(const   string     raw, bool for_postscript)
{
  unsigned char c = (unsigned) ((string &)raw)[1];
}
