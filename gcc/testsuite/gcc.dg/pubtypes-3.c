/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-O0 -gdwarf-2 -dA" } */
/* { dg-skip-if "Unmatchable assembly" { mmix-*-* } { "*" } { "" } } */
/* { dg-final { scan-assembler "__debug_pubtypes" } } */
/* { dg-final { scan-assembler "long+\[ \t\]+0x13b+\[ \t\]+\[#;]+\[ \t\]+Pub Info Length" } } */
/* { dg-final { scan-assembler "used_struct\\\\0\"+\[ \t\]+\[#;]+\[ \t\]+external name" } } */
/* { dg-final { scan-assembler-not "unused_struct\\\\0\"+\[ \t\]+\[#;]+\[ \t\]+external name" } } */
/* { dg-final { scan-assembler-not "\"list_name_type\\\\0\"+\[ \t\]+\[#;]+\[ \t\]+external name" } } */

#include <stdlib.h>
#include <stdio.h>

struct used_struct 
{
  int key;
  char *name;
};

struct unused_struct
{
  int key1;
  int f2;
  double f3;
  char *f4;
  struct unused_struct *next;
};

void 
foo (struct used_struct *list)
{
  enum list_name_type {
    boy_name,
    girl_name,
    unknown
  };

  int b_count = 0;
  int g_count = 0;
  int i;
  enum list_name_type *enum_list;

  enum_list = (enum list_name_type *) malloc (10 * sizeof (enum list_name_type));
  
  for (i = 0; i < 10; i++)
    {
      if (strncmp (list[i].name, "Alice", 5) == 0)
	{
	  enum_list[i] = girl_name;
	  g_count++;
	}
      else if (strncmp (list[i].name, "David", 5) == 0)
	{
	  enum_list[i] = boy_name;
	  b_count++;
	}
      else
	enum_list[i] = unknown;
    }

}

int
main (int argc, char **argv)
{
  int i;
  struct used_struct *my_list;

  my_list = (struct used_struct *) malloc (10 * sizeof (struct used_struct));
  
  for (i = 0; i < 10; i++)
    {
      my_list[i].key = i;
      my_list[i].name = (char *) malloc (11);
      sprintf (my_list[i].name, "Alice_%d", i);
    }

  foo (my_list);

  for (i = 0; i < 10; i++)
    fprintf (stdout, "Key: %d, Name: %s\n", my_list[i].key, my_list[i].name);
  
  return 0;
}
