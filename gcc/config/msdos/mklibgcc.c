#include <stdio.h>
#include <string.h>
#include <dirent.h>

char *skips[] = {
  "__main",
  "_ctors",
  "_exit",
  "_ctor_list",
  "_dtor_list",
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

  mkdir("lgcctmp", 0755);

  batfile = fopen("mklibnow.bat", "a");
  if (!batfile)
  {
    perror("mklibnow.bat");
    return 1;
  }
  fprintf(batfile, "@echo off\n");

  for (i=3; i<argc; i++)
  {
    char basename[30], *bp;
    int s;
    for (s=0; skips[s]; s++)
      if (strcmp(skips[s], argv[i]) == 0)
	break;
    if (skips[s])
      continue;
    strcpy(basename, "lgcctmp/");
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
    bp = basename + strlen(basename);
    fprintf(batfile, "%s -c %s.c -o %s.o\n", cc, basename, basename);
    strcpy(bp, ".c");
    cfile = fopen(basename, "w");
    if (cfile)
    {
      *bp = 0;
      fprintf(cfile, "#define L%s\n#include \"%s\"\n", argv[i], csrc);
      fclose(cfile);
    }
    else
      perror(basename);
  }

  fclose(batfile);
  return 0;
}
