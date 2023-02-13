#if defined(__WIN32__) && !defined(__CYGWIN__)
  /* Mostly skip on Windows, cf. main file why. */

int expect_open_to_fail () { return 1; }

void my_verify_not_exists (const char *dir) { }
void my_mkdir (const char *dir) { }
void my_rmdir (const char *dir) { }

#else

#include <sys/stat.h>  /* For mkdir + permission bits.  */
#include <unistd.h>  /* For rmdir.  */
#include <errno.h>  /* For errno.  */
#include <stdio.h>  /* For perror.  */
#include <stdlib.h>  /* For abort.  */
 

int expect_open_to_fail () { return 0; }

void
my_verify_not_exists (const char *dir)
{
  struct stat path_stat;
  int err = stat (dir, &path_stat);
  if (err && errno == ENOENT)
    return;  /* OK */
  if (err)
    perror ("my_verify_not_exists");
  else
    printf ("my_verify_not_exists: pathname %s still exists\n", dir);
  abort ();
 }

void
my_mkdir (const char *dir)
{
  int err;
  struct stat path_stat;

  /* Check whether 'dir' exists and is a directory.  */
  err = stat (dir, &path_stat);
  if (err && errno != ENOENT)
    {
      perror ("my_mkdir: failed to call stat for directory");
      abort ();
    }
  if (err == 0 && !S_ISDIR (path_stat.st_mode))
    {
      printf ("my_mkdir: pathname %s is not a directory\n", dir);
      abort ();
    }

  err = mkdir (dir, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
  if (err != 0)
    {
      perror ("my_mkdir: failed to create directory");
      abort ();
    }    
}

void
my_rmdir (const char *dir)
{
  rmdir (dir);
}

#endif  /* !defined(__WIN32__) || defined(__CYGWIN__) */
