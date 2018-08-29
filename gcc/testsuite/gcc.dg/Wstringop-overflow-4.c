/* { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow" } */

extern char* strchr (const char*, int);
extern char* strcpy (char*, const char*);
extern void* malloc (__SIZE_TYPE__);
extern __SIZE_TYPE__ strlen (const char *);
struct define_item {
    int len;
    char value[1];
};

struct define_item * foo(char *name)
{
  char * p;
  char * value;
  struct define_item * ptr;

  p = strchr (name, '=');
  if (1 && p) {
      value = p+1;
  } else
    value = "1";

  ptr = malloc(sizeof(struct define_item) + strlen(value));
  strcpy(ptr->value, value);  /* { dg-bogus "bytes into a region" } */
  return ptr;
}
