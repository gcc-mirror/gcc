/* { dg-do compile } */
/* { dg-options "-O1 -g" } */
extern int atoi (const char *);
extern int sprintf (char *, const char *, ...);
void malloc_init() {
  char *cptr;
  char buf[1];
  int tmbd = atoi(cptr);
  if (tmbd > 0)
    tmbd = (tmbd <= 124) ? tmbd : 124;
   else
    tmbd = 0;
   sprintf(buf, "%d\n", tmbd);
}
