#include <stdio.h>
#include <string.h>
#include <dirent.h>

char *skips[] = {
  0
};

int
do_clean()
{
  DIR *dir;
  struct dirent *de;
  remove("mklibnow.bat");

  dir = opendir("lgcctmp");
  if (!dir)
    return 0;
  while ((de=readdir(dir)))
    {
      char buf[30];
      if (de->d_name[0] == '.')
        continue;
      sprintf(buf, "lgcctmp/%s", de->d_name);
      unlink(buf);
    }
  closedir(dir);
  return 0;
}

int
main(int argc, char **argv)
{
  char *cc = argv[1];
  char *csrc=argv[2];
  int i;
  FILE *batfile;
  FILE *cfile;

  if (argc > 1 && strcmp(argv[1], "-c")==0)
    return do_clean();

  _mkdir("lgcctmp", 0755);

  batfile = fopen("mklibnow.bat", "a");
  if (!batfile)
    {
      perror("mklibnow.bat");
      return 1;
    }
/*  fprintf(batfile, "@echo off\n"); */

  for (i=3; i<argc; i++)
    {
      char dirname[30], basename[30], fullname[30], *bp;
      int s;
      for (s=0; skips[s]; s++)
        if (strcmp(skips[s], argv[i]) == 0)
  	break;
      if (skips[s])
        continue;
      strcpy(dirname, "lgcctmp/");
      strcpy(basename, "\0");
      if (strncmp(argv[i], "_fix", 4)==0)
        {
          strcat(basename, "fx");
          strcat(basename, argv[i]+4);
        }
      else if (strncmp(argv[i], "_float", 4)==0)
        {
          strcat(basename, "flt");
          strcat(basename, argv[i]+6);
        }
      else
        {
          strcat(basename, argv[i]);
        }
      strcpy (fullname, dirname);
      strcat (fullname, basename);
      fprintf(batfile, "%s -c lgcctmp/%s.c\n", cc, basename);
      fprintf(batfile, "copy %s.obj lgcctmp\n", basename);
      bp = fullname + strlen(fullname);
      strcpy(bp, ".c");
      cfile = fopen(fullname, "w");
      if (cfile)
        {
          *bp = 0;
          fprintf(cfile, "#define L%s\n#include \"%s\"\n", argv[i], csrc);
          fclose(cfile);
        }
      else
        perror(fullname);
    }

  fclose(batfile);
  return 0;
}
