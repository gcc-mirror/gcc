unsigned char lookup_table [257];

static int 
build_lookup (pattern)
     unsigned char *pattern;
{
  int m;

  m = strlen (pattern) - 1;
  
  memset (lookup_table, ++m, 257);
  return m;
}

int main(argc, argv)
     int argc;
     char **argv;
{
  if (build_lookup ("bind") != 4)
    abort ();
  else
    exit (0);
}

