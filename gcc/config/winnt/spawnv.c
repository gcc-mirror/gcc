/* This is a kludge to get around the Microsoft C spawn functions' propensity
   to remove the outermost set of double quotes from all arguments.  */

#define index(s,c) strchr((s),(c))

extern char *malloc ();

const char * const *
fix_argv (argv)
  char **argv;
{
  static char sh_chars[] = "\"";

  int i, len;
  char *new_argv;
  char *p, *ap;

  for (i=1; argv[i]; i++)
    {

      len = strlen (argv[i]);
      new_argv = malloc (2*len+3);
      ap = new_argv;

      for (p = argv[i]; *p != '\0'; ++p)
        {
          if (index (sh_chars, *p) != 0)
            *ap++ = '\\';
          *ap++ = *p;
        }
      *ap = '\0';
      argv[i] = new_argv;
    }

  return (const char * const *) argv;
}

int __spawnv (mode, cmdname, argv)
  int mode;
  const char *cmdname;
  char **argv;
{
  _spawnv (mode, cmdname, fix_argv (argv));
}

int __spawnvp (mode, cmdname, argv)
  int mode;
  const char *cmdname;
  char **argv;
{
  _spawnvp (mode, cmdname, fix_argv (argv));
}

int spawnve (mode, cmdname, argv, envp)
  int mode;
  const char *cmdname;
  char **argv;
  const char *const *envp;
{
  _spawnve (mode, cmdname, fix_argv (argv), envp);
}

int __spawnvpe (mode, cmdname, argv, envp)
  int mode;
  const char *cmdname;
  char **argv;
  const char *const *envp;
{
  _spawnvpe (mode, cmdname, fix_argv (argv), envp);
}

