/* getpwd.c - get the working directory */

#include "config.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef errno
extern int errno;
#endif

/* Virtually every UN*X system now in common use (except for pre-4.3-tahoe
   BSD systems) now provides getcwd as called for by POSIX.  Allow for
   the few exceptions to the general rule here.  */

#if !(defined (POSIX) || defined (USG) || defined (VMS)) || defined (HAVE_GETWD)
#include <sys/param.h>
extern char *getwd ();
#define getcwd(buf,len) getwd(buf)
#ifdef MAXPATHLEN
#define GUESSPATHLEN (MAXPATHLEN + 1)
#else
#define GUESSPATHLEN 100
#endif
#else /* (defined (USG) || defined (VMS)) */
extern char *getcwd ();
/* We actually use this as a starting point, not a limit.  */
#define GUESSPATHLEN 100
#endif /* (defined (USG) || defined (VMS)) */
#ifdef _WIN32
#include <direct.h>
#endif

char *getenv ();
char *xmalloc ();

#ifndef VMS

/* Get the working directory.  Use the PWD environment variable if it's
   set correctly, since this is faster and gives more uniform answers
   to the user.  Yield the working directory if successful; otherwise,
   yield 0 and set errno.  */

char *
getpwd ()
{
  static char *pwd;
  static int failure_errno;

  char *p = pwd;
  size_t s;
  struct stat dotstat, pwdstat;

  if (!p && !(errno = failure_errno))
    {
      if (! ((p = getenv ("PWD")) != 0
	     && *p == '/'
	     && stat (p, &pwdstat) == 0
	     && stat (".", &dotstat) == 0
	     && dotstat.st_ino == pwdstat.st_ino
	     && dotstat.st_dev == pwdstat.st_dev))

	/* The shortcut didn't work.  Try the slow, ``sure'' way.  */
	for (s = GUESSPATHLEN;  ! getcwd (p = xmalloc (s), s);  s *= 2)
	  {
	    int e = errno;
	    free (p);
#ifdef ERANGE
	    if (e != ERANGE)
#endif
	      {
		errno = failure_errno = e;
		p = 0;
		break;
	      }
	  }

      /* Cache the result.  This assumes that the program does
	 not invoke chdir between calls to getpwd.  */
      pwd = p;
    }
  return p;
}

#else	/* VMS */

#ifndef MAXPATHLEN
#define MAXPATHLEN 255
#endif

char *
getpwd ()
{
  static char *pwd = 0;

  if (!pwd) pwd = getcwd (xmalloc (MAXPATHLEN+1), MAXPATHLEN+1);
  return pwd;
}

#endif	/* VMS */
