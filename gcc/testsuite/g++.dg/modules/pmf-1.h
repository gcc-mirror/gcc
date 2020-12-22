
struct X 
{
  int mfn ();
};

inline void bob (X &)
{
  int (X::*pmf) () = &X::mfn;
}
