/* PR 21105 */

void
CheckFile ()
{
  char tagname[10];
  char *a = tagname;

  int validate ()
  {
    return (a == tagname + 4);
  }

  if (a == tagname)
    validate ();
}
