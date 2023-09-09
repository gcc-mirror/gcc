#include <stdlib.h>

/* Reduced from Juliet 1.3's CWE415_Double_Free__malloc_free_char_67a.c
   goodG2B which was showing a false leak report in a non-LTO build.  */

struct s1
{
    char * structFirst;
};
void external_fn_1(struct s1 myStruct);
void test_1()
{
    char * data;
    struct s1 myStruct;
    data = (char *)malloc(100*sizeof(char));
    if (data == NULL)
      exit(-1);
    myStruct.structFirst = data;
    external_fn_1(myStruct);
} /* { dg-bogus "leak of 'data'" } */

/* As above, but with padding before the field.  */

struct s2
{
  void *padding;
  char *ptr;
};
void external_fn_2(struct s2 myStruct);
void test_2()
{
    char * data;
    struct s2 myStruct;
    data = (char *)malloc(100*sizeof(char));
    if (data == NULL)
      exit(-1);
    myStruct.padding = NULL;
    myStruct.ptr = data;
    external_fn_2(myStruct);
} /* { dg-bogus "leak of 'data'" } */

