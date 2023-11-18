/* Reduced from a -Wanalyzer-infinite-loop false positive seen in Doom's
   d_main.c, which is under the GPLv2 or later.  */

extern char* wadfiles[20];

void
D_AddFile(const char* file)
{
  int numwadfiles;
  char* newfile;

  for (numwadfiles = 0; wadfiles[numwadfiles]; numwadfiles++) /* { dg-bogus "infinite loop" } */
    ;

  newfile = (char *)__builtin_malloc(__builtin_strlen(file) + 1);
  __builtin_strcpy(newfile, file); /* { dg-warning "possibly-NULL" } */

  wadfiles[numwadfiles] = newfile;
}

void
IdentifyVersion(void)
{
  D_AddFile("doom1.wad");
  D_AddFile("data_se/texture1.lmp");
}
