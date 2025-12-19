// PR c++/123030
// { dg-do compile { target c++11 } }

int n;
struct Y {
  Y () { if (++n == 2) throw 42; }
  ~Y () = delete;	// { dg-message "declared here" }
};

int
main ()
{
  try
    {
      new Y[2];		// { dg-error "use of deleted function 'Y::~Y\\\(\\\)'" }
    }
  catch (...)
    {
    }
}
