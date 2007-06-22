/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int *stderr;

void f (int *, const char *, ...);

void g (const char *conf_name)
{
  typedef struct
  {
    const char *label;
    const int value;
  } Section;

  const Section sections[2] = { {"", 0}, {"", 1} };

  f (stderr, "", "", conf_name, 0, sections[0]);
  f (stderr, "", "", conf_name, 0, sections[0]);
}
