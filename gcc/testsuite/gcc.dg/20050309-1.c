/* This caused an ICE on s390 due to incorrect secondary
   output reloads.  */

/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O2 -fprofile-generate" } */

char *
test(char *ret, int *counter, void *schema,
     const char* name, const char *namespace,
     void *node, int topLevel)
{
  char buf[30];
  int val;

  if (counter == 0) return 0;
  if (schema == 0) return 0;
  if (name == 0) return 0;

  __builtin_memset (ret, 0, 100);
  lookup (schema, name, -1);
  val = hash (schema, name, namespace, name, ret);
  if (val == 0) return ret;

  if (topLevel != 0)
    {
      error (1, 0, 0, node, "%s", name);
      return 0;
    }

  __snprintf_chk (buf, 29, 1, 30, "#eCont %d", ++*counter);
  val = hash (schema, name, buf, namespace, ret);
  if (val == 0) return ret;

  error (1, 0, 0, node, "%s", name);
  return 0;
}
