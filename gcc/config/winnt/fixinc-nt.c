#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>

static char *concat();
static char *concat3();
static char *concat4();
static int onlyonedir;
static int atleastone;
static char *fixeddirs, *origdirs;

/* Convert all /'s to \'s */

char *
slash2slash (dirname)
  char *dirname;
{
  int i;
  for (i=0; dirname[i]; i++)
    if (dirname [i] == '/')
      dirname [i] = '\\';

  return dirname;
}

/* Examine each directory component of a path and create the directory */

int
mkdirpath (dirpath)
  char *dirpath;
{
  char *ndirpath = strdup (dirpath);
  char *bp, *fp;

  fp = bp = ndirpath;

  while (bp) 
    {
      bp = strchr (fp, '\\');
      if (bp)
        {
          *bp = 0;
          _mkdir (ndirpath);
          *bp = '\\';
          fp = ++bp;
        }
      else
        _mkdir (ndirpath);
    }
}

/* Construct a relative directory path from a given path by removing the
   leading slash, if it exists and changing a drive letter from X: to X-. */

char *
newname (olddirname)
  char *olddirname;
{
  char *newname = strdup (olddirname);

  if ((strlen (newname) >= 2)
      && (isalpha (newname[0]) && newname[1] == ':'))
    newname [1] = '-';
  else if ((strlen (newname) >= 1) 
           && (newname [0] == '/' || newname [0] == '\\'))
    newname = &newname[1];

  return newname;
 
}

/* Run the sed script on one header file.  If no modifications were made, then
   delete the newly created file. */

int
doheader (oneheader, outheader, oldsize)
  char *oneheader, *outheader;
  int oldsize;
{
  char *newbuff, *oldbuff;
  char *newheader = concat3 ("include", "\\", newname (outheader));
  struct _stat newstatbuf;
  int newdesc, olddesc;
  int i;

  system (concat4 ("sed -f fixinc-nt.sed ", oneheader, " > ", newheader));
  _stat (newheader, &newstatbuf);
  if (oldsize != newstatbuf.st_size) 
    {
      atleastone = 1;
      printf ("Fixing: %s\n", oneheader);
      return 0;
    }
  oldbuff = malloc (oldsize);
  newbuff = malloc (newstatbuf.st_size);
  olddesc = open (oneheader, _O_RDONLY | _O_BINARY);
  newdesc = open (newheader, _O_RDONLY | _O_BINARY);
  read (olddesc, oldbuff, oldsize);
  read (newdesc, newbuff, newstatbuf.st_size);
  close (olddesc);
  close (newdesc);
  for (i=0; i<oldsize; i++)
    {
      if (oldbuff [i] != newbuff [i])
        {
          free (oldbuff);
          free (newbuff);
          atleastone = 1;
          printf ("Fixing: %s\n", oneheader);
          return 0;
        }
    }
  free (oldbuff);
  free (newbuff);
  unlink (newheader);
  return 0;
  
}

/* Examine the contents of a directory and call doheader () for a regular file
   and recursively call dodir () for an enclosed directory. */

int
dodir (indir, outdir)
  char *indir, *outdir;
{
  DIR *dir;
  struct dirent *dire;
  struct _stat statbuf;
  char *intempbuf, *outtempbuf;

  dir = opendir (indir);
  if (!dir) return 0;

  mkdirpath (concat3 ("include", "\\", newname (outdir)));
  while ((dire = readdir (dir)))
    {
      if (dire->d_name[0] == '.')
        continue;
  
      intempbuf = slash2slash (concat3 (indir, "\\", dire->d_name));
      outtempbuf = slash2slash (concat3 (outdir, "\\", dire->d_name));
      _stat (intempbuf, &statbuf);
  
      /* If directory ... */
      if (statbuf.st_mode & _S_IFDIR)
        dodir (intempbuf, outtempbuf);
  
      /* If regular file ... */
      if (statbuf.st_mode & _S_IFREG)
        doheader (intempbuf, outtempbuf, statbuf.st_size);
    }
  closedir (dir);
  return 0;
}

/* Retrieve the value of the Include environment variable, copy it into a
   temporary and append a semi-colon for book-keeping purposes. Then call
   dodir () for each complete directory that is named therein.  If there is
   only one directory, then direct the output to use include\. as the
   root instead of include/<directory path>, where <directory path> is a path
   constructed from the path named in the Include environment variable.  
   I.e. if Include=C:\MSTOOLS\Include;D:\MSVC20\Include then the modified
   header files will be in include\C-\MSTOOLS\Include and 
   include\D-\MSVC20\Include.  However if Include=C:\MSTOOLS\Include then the
   modified files will be in include\. */

int
main ()
{
  char *fp, *bp, *foobar;
  char *incvar = getenv ("Include");
  int varlen = 0;
  struct _stat statbuf;

  if (incvar == NULL) return 0;

  varlen = strlen (incvar);
  foobar = (char *) malloc (varlen + 2);

  strcpy (foobar, incvar);
  foobar = slash2slash (foobar);
  if (foobar [varlen-1] != ';') strcat (foobar, ";");
  fp = bp = foobar;

  if (strchr (fp, ';') == strrchr (fp, ';'))
    onlyonedir = 1;
  else
    onlyonedir = 0;

  fixeddirs = strdup(".\\include");
  origdirs = strdup("");

  while (bp)
    {
      bp = strchr (fp, ';');
      if (bp)
        {
          *bp = 0;
          _stat (fp, &statbuf);
          if (statbuf.st_mode & _S_IFDIR)
            {
              atleastone = 0;
              if (onlyonedir) 
                dodir (fp, ".");
              else
                dodir (fp, fp);
              if (atleastone && !onlyonedir)
                {
                  origdirs = concat3 (origdirs, ";", fp);
                  fixeddirs = concat3 (fixeddirs, ";", 
                    concat3 (".\\include", "\\", newname(fp)));
                }
            }
          fp = ++bp;
        }
    }
  printf ("set C_Include_Path=%s%s\n", fixeddirs, origdirs);
  return 0;
}

/* Utility function that mallocs space and concatenates two strings. */

static char *
concat (s1, s2)
     char *s1, *s2;
{
  int len1 = strlen (s1);
  int len2 = strlen (s2);
  char *result = malloc (len1 + len2 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  *(result + len1 + len2) = 0;

  return result;
}

/* Utility function that concatenates three strings. */

static char *
concat3 (s1, s2, s3)
     char *s1, *s2, *s3;
{
  return concat (concat (s1, s2), s3);
}

/* Utility function that concatenates four strings. */

static char *
concat4 (s1, s2, s3, s4)
     char *s1, *s2, *s3, *s4;
{
  return concat (concat (s1, s2), concat (s3, s4));
}
