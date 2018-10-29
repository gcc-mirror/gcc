/* PR sanitizer/85774 */
/* { dg-do run } */

#include <functional>

void
DoSomething ()
{
}

void
DoFunc (const std::function<void(void)> &func)
{
  func ();
}

void
Setup ()
{
  switch (1)
    {
    case 1:
      {
	DoFunc ([]() {});
	break;
      }
    case 2:
      {
	DoFunc ([]() {});
	break;
      }
    default:
      break;
    }

  DoSomething ();
}

void
DemostrateBadPoisoning ()
{
  DoFunc ([]() {});
}

int
main ()
{
  Setup ();
  DemostrateBadPoisoning ();
  return 0;
}
