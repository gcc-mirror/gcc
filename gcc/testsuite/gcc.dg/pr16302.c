/* PR 16302 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */
void bar (int);
int
foo (int argc, char *argv[])
{
  if (argc != 1 || argc != 2) return 1; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  if (argc < 0 && argc > 10) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  if (argc || !argc) return 1; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  if (argc && !argc) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc != 1 || argc != 2); /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  bar (argc < 0 && argc > 10); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc || !argc); /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  bar (argc && !argc); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc != 1 || argc != 2) ? 1 : 0 ; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  return (argc < 0 && argc > 10) ? 1 : 0; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc || !argc) ? 1 : 0; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  return (argc && !argc) ? 1 : 0; /* { dg-warning "'and' of mutually exclusive tests is always false" } */

  if (argc == 2 && argc == 1) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  if (argc < 0 && argc > 10) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  if (argc || !argc) return 1; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  if (argc && !argc) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc == 2 && argc == 1); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc < 0 && argc > 10); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc || !argc); /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  bar (argc && !argc); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc == 2 && argc == 1) ? 1 : 0 ; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc < 0 && argc > 10) ? 1 : 0; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc || !argc) ? 1 : 0; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  return (argc && !argc) ? 1 : 0; /* { dg-warning "'and' of mutually exclusive tests is always false" } */

  if (argc == 2 && argc == 1) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  if (argc < 0 && argc > 10) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  if (!argc || argc) return 1; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  if (!argc && argc) return 1; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc == 2 && argc == 1); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (argc < 0 && argc > 10); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  bar (!argc || argc); /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  bar (!argc && argc); /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc == 2 && argc == 1) ? 1 : 0 ; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (argc < 0 && argc > 10) ? 1 : 0; /* { dg-warning "'and' of mutually exclusive tests is always false" } */
  return (!argc || argc) ? 1 : 0; /* { dg-warning "'or' of collectively exhaustive tests is always true" } */
  return (!argc && argc) ? 1 : 0; /* { dg-warning "'and' of mutually exclusive tests is always false" } */

  return 0;
}

int
foo2 (int argc)
{
  if (5 != 1 || 5 != 2) return 1;
  if (5 < 0 && 5 > 10) return 1;
  if (1 || 0) return 1;
  if (0 && 1) return 1;
  if (2 || !2) return 1;
  if (2 && !2) return 1;
  if (!(!2) || !(2)) return 1;
  if (!(!2) && !(2)) return 1;
  bar (5 != 1 || 5 != 2);
  bar (5 < 0 && 5 > 10);
  bar (1 || 0);
  bar (0 && 1);
  bar (2 || !2);
  bar (2 && !2);
  return (5 != 1 || 5 != 2) ? 1 : 0 ;
  return (5 < 0 && 5 > 10) ? 1 : 0;
  return (1 || 0) ? 1 : 0 ;
  return (0 && 1) ? 1 : 0;
  return (2 || !2) ? 1 : 0;
  return (2 && !2) ? 1 : 0;

  return 0;
}

