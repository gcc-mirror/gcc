/* Test compilation of typedef composition in structs.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

typedef struct
{
  int day, month, year;
} Date;

typedef struct
{
  Date filedDate, fixedDate;
  int severity;
} BugRef;

BugRef CR2112;
