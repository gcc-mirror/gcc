/*
 * @(#)msd_dir.c 1.4 87/11/06	Public Domain.
 *
 *  A public domain implementation of BSD directory routines for
 *  MS-DOS.  Written by Michael Rendell ({uunet,utai}michael@garfield),
 *  August 1897
 *
 *  Modified by Ian Stewartson, Data Logic (istewart@datlog.co.uk).
 *
 *  Updates:  1.  To support OS/2 1.x
 *	      2.  To support HPFS long filenames
 *	      3.  To support OS/2 2.x
 *	      4.  To support TurboC
 *	      5.  To support Windows NT
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

#include <malloc.h>

#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>


#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define FILE_NAME_E		cFileName
#define OS_CloseFH(a)		FindClose (a)
#define FIND_BUFFER		WIN32_FIND_DATA
#define DISABLE_HARD_ERRORS	SetErrorMode (0)
#define ENABLE_HARD_ERRORS	SetErrorMode (SEM_FAILCRITICALERRORS | \
					      SEM_NOOPENFILEERRORBOX);

#  define ERROR_EMPTY_DIR	ERROR_FILE_NOT_FOUND

#  define ATTRIBUTES		(_A_SUBDIR | _A_HIDDEN | _A_SYSTEM | \
				 _A_NORMAL | _A_RDONLY | _A_ARCH)

/*
 * missing ??
 */

#ifndef ENOTDIR
#  define ENOTDIR	120	/* Not a directory			*/
#endif

#ifndef S_IFMT
#  define	S_IFMT	0xf000	/* type of file				*/
#endif

#ifndef S_ISDIR
#  define S_ISDIR(m)	((((m) & S_IFMT) == S_IFDIR))
#endif

/*
 * Internals
 */

typedef struct _dircontents	DIRCONT;
static void			free_dircontents (DIRCONT *);

/*
 * Open the directory stream
 */

DIR *
opendir (name)
    const char	*name;
{
    struct stat		statb;
    DIR			*dirp;
    char		*last;
    DIRCONT		*dp;
    char		*nbuf;
    int			len = strlen (name);
    unsigned long	rc;
    FIND_BUFFER		dtabuf;
    HANDLE		d_handle;
    bool		HPFS = FALSE;

    if (!len)
    {
	errno = ENOTDIR;
	return (DIR *)NULL;
    }

    if ((nbuf = malloc (len + 5)) == (char *)NULL)
	return (DIR *) NULL;

    strcpy (nbuf, name);
    last = &nbuf[len - 1];

/* Ok, DOS is very picky about its directory names.  The following are
 * valid.
 *
 *  c:/
 *  c:.
 *  c:name/name1
 *
 *  c:name/ is not valid
 */

    if (((*last == '\\') || (*last == '/')) && (len > 1) &&
	(!((len == 3) && (name[1] == ':'))))
	*(last--) = 0;

/* Check its a directory */

    DISABLE_HARD_ERRORS;
    rc = stat (nbuf, &statb);
    ENABLE_HARD_ERRORS;

    if (rc)
    {
	free (nbuf);
	return (DIR *) NULL;
    }

    if (!S_ISDIR (statb.st_mode))
    {
	free (nbuf);
	errno = ENOTDIR;
	return (DIR *)NULL;
    }

    if ((dirp = (DIR *) malloc (sizeof (DIR))) == (DIR *) NULL)
    {
	free (nbuf);
	return (DIR *) NULL;
    }

/* Set up to find everything */

    if ((*last != '\\') && (*last != '/'))
	strcat (last, "/");

    strcat (last, "*.*");

/* Find the file system type */

    HPFS = IsHPFSFileSystem (nbuf);

    dirp->dd_loc      = 0;
    dirp->dd_cp       = (DIRCONT *) NULL;
    dirp->dd_contents = (DIRCONT *) NULL;

    DISABLE_HARD_ERRORS;

    d_handle = FindFirstFile (nbuf, &dtabuf);
    rc = (d_handle == INVALID_HANDLE_VALUE) ? GetLastError () : 0;

    ENABLE_HARD_ERRORS;

/* Check for errors */

    if (rc)
    {
	free (nbuf);

/* Empty directory */

#if defined (ERROR_EMPTY_DIR)
	if (rc == ERROR_EMPTY_DIR)
	    return dirp;
#endif

	free (dirp);
	return (DIR *) NULL;
    }

/* Process the directory */

    do
    {
	if (((dp = (DIRCONT *) malloc (sizeof (DIRCONT))) == (DIRCONT *)NULL) ||
	    ((dp->_d_entry = strdup (dtabuf.FILE_NAME_E)) == (char *) NULL))
	{
	    if (dp->_d_entry != (char *)NULL)
		free ((char *)dp);

	    free (nbuf);
	    free_dircontents (dirp->dd_contents);

	    OS_CloseFH (d_handle);
	    return (DIR *) NULL;
	}

	if (!HPFS)
	    strlwr (dp->_d_entry);

	if (dirp->dd_contents != (DIRCONT *) NULL)
	    dirp->dd_cp = dirp->dd_cp->_d_next = dp;

	else
	    dirp->dd_contents = dirp->dd_cp = dp;

	dp->_d_next = (DIRCONT *) NULL;

    } while (FindNextFile (d_handle, &dtabuf));

    dirp->dd_cp = dirp->dd_contents;
    free (nbuf);

    OS_CloseFH (d_handle);
    return dirp;
}


/*
 * Close the directory stream
 */

int
closedir (dirp)
    DIR *dirp;
{
    if (dirp != (DIR *)NULL)
    {
	free_dircontents (dirp->dd_contents);
	free ((char *)dirp);
    }

    return 0;
}

/*
 * Read the next record from the stream
 */

struct dirent *
readdir (dirp)
    DIR	*dirp;
{
    static struct dirent	dp;

    if ((dirp == (DIR *)NULL) || (dirp->dd_cp == (DIRCONT *) NULL))
	return (struct dirent *) NULL;

    dp.d_reclen = strlen (strcpy (dp.d_name, dirp->dd_cp->_d_entry));
    dp.d_off    = dirp->dd_loc * 32;
    dp.d_ino    = (ino_t)++dirp->dd_loc;
    dirp->dd_cp = dirp->dd_cp->_d_next;

    return &dp;
}

/*
 * Restart the directory stream
 */

void
rewinddir (dirp)
    DIR *dirp;
{
    seekdir (dirp, (off_t)0);
}

/*
 * Move to a know position in the stream
 */

void
seekdir (dirp, off)
    DIR *dirp;
    off_t off;
{
    long	i = off;
    DIRCONT	*dp;

    if ((dirp == (DIR *)NULL) || (off < 0L))
	return;

    for (dp = dirp->dd_contents; (--i >= 0) && (dp != (DIRCONT *)NULL);
	 dp = dp->_d_next)
	;

    dirp->dd_loc = off - (i + 1);
    dirp->dd_cp = dp;
}

/*
 * Get the current position
 */

off_t
telldir(dirp)
    DIR *dirp;
{
    return (dirp == (DIR *)NULL) ? (off_t) -1 : dirp->dd_loc;
}

/*
 * Release the internal structure
 */

static void
free_dircontents (dp)
    DIRCONT *dp;
{
    DIRCONT	*odp;

    while ((odp = dp) != (DIRCONT *)NULL)
    {
	if (dp->_d_entry != (char *)NULL)
	    free (dp->_d_entry);

	dp = dp->_d_next;
	free ((char *)odp);
    }
}


/*
 * Windows NT version
 */

bool
IsHPFSFileSystem (directory)
    char *directory;
{
    char		bName[4];
    DWORD		flags;
    DWORD		maxname;
    BOOL		rc;
    unsigned int	nDrive;
    char		szCurDir [MAX_PATH];

    if (isalpha (directory[0]) && (directory[1] == ':'))
	nDrive = toupper (directory[0]) - '@';

    else
    {
	GetCurrentDirectory (MAX_PATH, szCurDir);
	nDrive = szCurDir[0] - 'A' + 1;
    }

/* Set up the drive name */

    strcpy (bName, "x:\\");
    bName[0] = (char) (nDrive + '@');

/* Read the volume info, if we fail - assume non-HPFS */

    DISABLE_HARD_ERRORS;

    rc = GetVolumeInformation (bName, (LPTSTR)NULL, 0, (LPDWORD)NULL,
			       &maxname, &flags, (LPTSTR)NULL, 0);
    ENABLE_HARD_ERRORS;

    return ((rc) && (flags & (FS_CASE_SENSITIVE | FS_CASE_IS_PRESERVED)))
    		? TRUE : FALSE;
}

