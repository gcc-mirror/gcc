/* Helper for pr128183.f90.  */

#include <locale.h>
#include <string.h>

static char saved_name[256];

/* Set LC_NUMERIC to an installed locale whose name is not "C".  Windows names
   come first, since targets without uselocale are mostly MinGW-w64.  Return 0
   if there is none.  */

int
set_non_c_locale (void)
{
  static const char *const names[] = {
    "French", "German", "English", "C.UTF-8", "C.utf8", NULL
  };

  for (int i = 0; names[i] != NULL; i++)
    {
      const char *name = setlocale (LC_NUMERIC, names[i]);
      if (name != NULL && strcmp (name, "C") != 0
	  && strlen (name) < sizeof saved_name)
	{
	  strcpy (saved_name, name);
	  return 1;
	}
    }
  return 0;
}

/* Return 1 if LC_NUMERIC is still the locale set by set_non_c_locale.  */

int
locale_unchanged (void)
{
  const char *name = setlocale (LC_NUMERIC, NULL);
  return name != NULL && strcmp (name, saved_name) == 0;
}
