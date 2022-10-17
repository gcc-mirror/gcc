/* { dg-do "compile" } */

#define NULL ((void *) 0)
typedef struct _IO_FILE FILE;

extern char *fgets(char *__restrict __s, int __n,
		   FILE *__restrict __stream);
extern char *fgets_unlocked(char *__restrict __s, int __n,
			    FILE *__restrict __stream);

char
test_1 (FILE *fp)
{
  char buf[400];

  if (fgets (buf, sizeof buf, fp) == NULL)
    return 0;

  return buf[0];
}

char
test_2 (FILE *fp)
{
  char buf[400];

  if (fgets_unlocked (buf, sizeof buf, fp) == NULL)
    return 0;

  return buf[0];
}
