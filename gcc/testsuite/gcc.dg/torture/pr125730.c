/* { dg-do run } */

char *key;
char *value;
char **name_regex = &key;

__attribute__ ((noipa)) void
dict_get_item_ref (char ***result)
{
  *result = &value;
}

__attribute__ ((noinline)) static void
parse_keyword_dict (char ***argnames, char ***values,
		    int num_posargs, int num_kwargs)
{
  char ***first_kw_arg = argnames + num_posargs;
  char ***name = first_kw_arg;

  while (*name && num_kwargs > 0)
    {
      char **fetched_value;
      dict_get_item_ref (&fetched_value);
      values[name - argnames] = fetched_value;
      name++;
    }
}

int
main ()
{
  char **fetched_values[] = { 0 };
  char **argnames[] = { name_regex, 0 };
  parse_keyword_dict (argnames, fetched_values, 0, 1);

  if (fetched_values[0] != &value)
    __builtin_abort ();

  return 0;
}
