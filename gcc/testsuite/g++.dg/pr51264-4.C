/* { dg-do compile } */
/* { dg-options "-O0 -Werror -Wreturn-type" } */

/* Test-case from http://gcc.gnu.org/bugzilla/show_bug.cgi?id=25973#c4.  */

struct Block
{
  public:
  Block ();
  ~Block ();
};

bool func (bool bar)
{
  Block block;
  bool foo = false;

  if (!foo || bar)
    do
      {
	return true;
      }
    while (0);
  else
    do
      {
	return false;
      }
    while (0);
}
