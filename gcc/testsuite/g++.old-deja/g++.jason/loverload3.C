// PRMS Id: 2010
// Bug: g++ doesn't deal with overloads involving C-language fns properly.
// Build don't link:

extern "C" double pow (double, double);
inline double pow (double d, int e) { return pow (d, (double) e); }

void foo () 
{
  pow (1.0, 1);
  pow (1.0, 1.0);
}
