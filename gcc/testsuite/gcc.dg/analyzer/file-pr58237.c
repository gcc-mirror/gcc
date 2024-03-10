/* C only: C++ exceptions double the fopen leak warnings.
   Therefore this test has been duplicated as
   c-c++-common/analyzer/file-pr58237-noexcept.c  */

typedef struct FILE   FILE;

FILE* fopen (const char*, const char*);
int   fclose (FILE*);
char *fgets (char *, int, FILE *);

#include "analyzer-decls.h"

void f0(const char *str)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  // ideally warning should be located at the end of the function
  char buf[10];
  fgets(buf, 10, fp);
} /* { dg-warning "leak of FILE 'fp'" } */

void f1(const char *str)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  // ideally warning should be located at the end of the function
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
} /* { dg-warning "leak of FILE 'fp'" } */

void f2(const char *str, int flag)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  // ideally warning should be located at the end of the function
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
  if (flag) /* { dg-message "when 'flag == 0'" } */
    fclose(fp);
} /* { dg-warning "leak of FILE 'fp'" } */

extern void called_by_f3( FILE * fp);

void f3(const char *str)
{
  FILE * fp = fopen(str, "r");
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
  /* Not sure if fclose executed by called_by_f3 or not. Say nothing */
  called_by_f3(fp);
}

void f4(const char *str)
{
  FILE * fp = fopen(str, "r");
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
  /* Nothing to say here. */
  fclose(fp);
}

int main(int argc, const char * argv[])
{
  FILE * fp = fopen(argv[0], "r");
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
  /* Nothing to say here, because we are in main. */
}
