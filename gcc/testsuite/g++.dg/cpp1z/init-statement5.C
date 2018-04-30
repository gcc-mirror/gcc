// Testcase from P0305R1
// { dg-options -std=c++17 }

enum class status_code { SUCCESS };
extern int get_value ();
status_code bar (int);
status_code do_more_stuff (void);

status_code
foo ()
{
  int n = get_value ();
  if (status_code c = bar (n); c != status_code::SUCCESS) { return c; }
  if (status_code c = do_more_stuff (); c != status_code::SUCCESS) { return c; }
  return status_code::SUCCESS;
}
