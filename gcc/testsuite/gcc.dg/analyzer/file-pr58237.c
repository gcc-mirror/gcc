#include <stdio.h>

void f0(const char *str)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  char buf[10];
  fgets(buf, 10, fp);
} /* { dg-warning "leak of FILE 'fp'" } */

void f1(const char *str)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
} /* { dg-warning "leak of FILE 'fp'" } */

void f2(const char *str, int flag)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
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

void main(int argc, const char * argv[])
{
  FILE * fp = fopen(argv[0], "r");
  char buf[10];

  while (fgets(buf, 10, fp) != NULL)
    {
      /* Do something with buf */
    }
  /* Nothing to say here, because we are in main. */
}
