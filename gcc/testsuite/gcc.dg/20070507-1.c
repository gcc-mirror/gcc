/* This failed on s390x due to bug in loop.c.
   loop.c failed to remove a REG_EQUAL note when
   hoisting an insn from a loop body.  */

/* { dg-options "-O3 -fPIC" } */
/* { dg-do run { target fpic } } */

typedef __SIZE_TYPE__ size_t;
int memcmp(const void *s1, const void *s2, size_t n);

typedef struct
{
  char name[30];
  int a;
} LOCAL;

int global = 0;
int sy = 1;
int subroutine_offset;

LOCAL local = { "local", 0 };
LOCAL keywords = { "keywords", 1 };
int local_table = 0;
int keywords_table = 0;

void __attribute__((noinline)) bar (char *p_buffer)
{
  p_buffer[255] = 1;
}

int __attribute__((noinline)) foo (char *p_str1)
{
  global = 1;
  return 1;
}

int __attribute__((noinline)) loop_next (int *p_table, char *p_table_head)
{
  static loop_next = 0;

  if (loop_next == 1)
    return 1;

  loop_next = 1;
  return 0;
}

int
main ()
{
  char buffer[256];
  int ende = 0;
  int index;
  int local_base = 2;

  keywords.a = 1;
  for (sy = 0;; sy++)
    {
      for (index = 1;;)
	{
	  bar (buffer);
	  if (buffer[sy] != 0)
	    {
	      ende = 1;
	      break;
	    };
	  if (foo (buffer))
	    {
	      keywords.a += index - 1;
	      break;
	    }
	  index++;
	}
      if (ende)
	break;
    }

  subroutine_offset = 0;

  for (;;)
    {
      if (loop_next (&keywords_table, (char*)&keywords))
	break;

      if ((!memcmp (keywords.name, "+++", 3)))
	local_base = 100;
      else
	local_base = 0;

      if ((!memcmp (keywords.name, "+++", 3)))
	subroutine_offset += local_table;

      for (;;)
	{
	  if (loop_next (&local_table, (char*)&local))
	    break;;
	  if ((local.a == 0))
	    continue;;
	  foo (local.name);
	}
    }
  return 0;
}
