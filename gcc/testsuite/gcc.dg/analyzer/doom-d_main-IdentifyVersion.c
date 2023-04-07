/* Reduced from Doom's d_main.c, which is under the GPLv2 or later.  */

/* { dg-additional-options "-Wno-analyzer-too-complex" } */

typedef __SIZE_TYPE__ size_t;
typedef struct _IO_FILE FILE;

extern int
printf(const char* __restrict, ...);

extern int
sprintf(char* __restrict __s, const char* __restrict, ...)
  __attribute__((__nothrow__));

extern void*
malloc(size_t __size) __attribute__((__nothrow__, __leaf__))
__attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
extern char*
getenv(const char* __name) __attribute__((__nothrow__, __leaf__))
__attribute__((__nonnull__(1)));
extern int
access(const char* __name, int __type) __attribute__((__nothrow__, __leaf__))
__attribute__((__nonnull__(1)));
extern char*
strcpy(char* __restrict __dest, const char* __restrict __src)
  __attribute__((__nothrow__, __leaf__)) __attribute__((__nonnull__(1, 2)));
extern size_t
strlen(const char* __s) __attribute__((__nothrow__, __leaf__))
__attribute__((__pure__)) __attribute__((__nonnull__(1)));

typedef enum
{
  shareware,
  registered,
  commercial,

  retail,
  indetermined

} GameMode_t;

typedef enum
{
  doom,
  doom2,
  pack_tnt,
  pack_plut,
  none

} GameMission_t;

typedef enum
{
  english,
  french,
  german,
  unknown

} Language_t;

typedef enum
{
  false,
  true
} boolean;

extern boolean devparm;
extern GameMode_t gamemode;
extern Language_t language;
extern char basedefault[1024];
int
M_CheckParm(char* check);
void
I_Error(char* error, ...);

extern char* wadfiles[20];

void
D_AddFile(char* file)
{
  int numwadfiles;
  char* newfile;

  for (numwadfiles = 0; wadfiles[numwadfiles]; numwadfiles++)
    ;

  newfile = malloc(strlen(file) + 1);
  strcpy(newfile, file); /* { dg-warning "use of possibly-NULL 'newfile' where non-null expected" } */

  wadfiles[numwadfiles] = newfile;
}

void
IdentifyVersion(void)
{

  char* doom1wad;
  char* doomwad;
  char* doomuwad;
  char* doom2wad;

  char* doom2fwad;
  char* plutoniawad;
  char* tntwad;

  char* home;
  char* doomwaddir;
  doomwaddir = getenv("DOOMWADDIR");
  if (!doomwaddir)
    doomwaddir = ".";

  doom2wad = malloc(strlen(doomwaddir) + 1 + 9 + 1);
  sprintf(doom2wad, "%s/doom2.wad", doomwaddir); /* { dg-warning "possibly-NULL 'doom2wad'" } */

  doomuwad = malloc(strlen(doomwaddir) + 1 + 8 + 1);
  sprintf(doomuwad, "%s/doomu.wad", doomwaddir); /* { dg-warning "possibly-NULL 'doomuwad'" } */

  doomwad = malloc(strlen(doomwaddir) + 1 + 8 + 1);
  sprintf(doomwad, "%s/doom.wad", doomwaddir); /* { dg-warning "possibly-NULL 'doomwad'" } */

  doom1wad = malloc(strlen(doomwaddir) + 1 + 9 + 1);
  sprintf(doom1wad, "%s/doom1.wad", doomwaddir); /* { dg-warning "possibly-NULL 'doom1wad'" } */

  plutoniawad = malloc(strlen(doomwaddir) + 1 + 12 + 1);
  sprintf(plutoniawad, "%s/plutonia.wad", doomwaddir); /* { dg-warning "possibly-NULL 'plutoniawad'" } */

  tntwad = malloc(strlen(doomwaddir) + 1 + 9 + 1);
  sprintf(tntwad, "%s/tnt.wad", doomwaddir); /* { dg-warning "possibly-NULL 'tntwad'" } */

  doom2fwad = malloc(strlen(doomwaddir) + 1 + 10 + 1);
  sprintf(doom2fwad, "%s/doom2f.wad", doomwaddir); /* { dg-warning "possibly-NULL 'doom2fwad'" } */

  home = getenv("HOME");
  if (!home)
    I_Error("Please set $HOME to your home directory");
  sprintf(basedefault, "%s/.doomrc", home);

  if (M_CheckParm("-shdev")) {
    gamemode = shareware;
    devparm = true;
    D_AddFile("devdata"
              "doom1.wad");
    D_AddFile("devmaps"
              "data_se/texture1.lmp");
    D_AddFile("devmaps"
              "data_se/pnames.lmp");
    strcpy(basedefault,
           "devdata"
           "default.cfg");
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (M_CheckParm("-regdev")) {
    gamemode = registered;
    devparm = true;
    D_AddFile("devdata"
              "doom.wad");
    D_AddFile("devmaps"
              "data_se/texture1.lmp");
    D_AddFile("devmaps"
              "data_se/texture2.lmp");
    D_AddFile("devmaps"
              "data_se/pnames.lmp");
    strcpy(basedefault,
           "devdata"
           "default.cfg");
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (M_CheckParm("-comdev")) {
    gamemode = commercial;
    devparm = true;

    D_AddFile("devdata"
              "doom2.wad");

    D_AddFile("devmaps"
              "cdata/texture1.lmp");
    D_AddFile("devmaps"
              "cdata/pnames.lmp");
    strcpy(basedefault,
           "devdata"
           "default.cfg");
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (!access(doom2fwad, 4)) {
    gamemode = commercial;

    language = french;
    printf("French version\n");
    D_AddFile(doom2fwad);
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (!access(doom2wad, 4)) {
    gamemode = commercial;
    D_AddFile(doom2wad);
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (!access(plutoniawad, 4)) {
    gamemode = commercial;
    D_AddFile(plutoniawad);
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (!access(tntwad, 4)) {
    gamemode = commercial;
    D_AddFile(tntwad);
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  if (!access(doomuwad, 4)) {
    gamemode = retail;
    D_AddFile(doomuwad);
    return; /* { dg-warning "leak of 'doom2wad'" } */
    /* { dg-warning "leak of 'doomuwad'"    "leak" { target *-*-* } .-1 } */
    /* { dg-warning "leak of 'doomwad'"     "leak" { target *-*-* } .-2 } */
    /* { dg-warning "leak of 'doom1wad'"    "leak" { target *-*-* } .-3 } */
    /* { dg-warning "leak of 'plutoniawad'" "leak" { target *-*-* } .-4 } */
    /* { dg-warning "leak of 'tntwad'"      "leak" { target *-*-* } .-5 } */
    /* { dg-warning "leak of 'doom2fwad'"   "leak" { target *-*-* } .-6 } */
  }

  /* [...snip...] */

  printf("Game mode indeterminate.\n");
  gamemode = indetermined;
}
