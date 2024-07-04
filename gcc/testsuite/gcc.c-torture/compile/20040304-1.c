int macarg (char *, int);
void
cpplib_macroExpand (char * pfile)
{
  int nargs;
  int rest_args;
  int token = -1;
  rest_args = 0;
  do
    {
      if (rest_args != 0)
          continue;
      if (nargs == 0)
        {
          rest_args = 1;
          token = macarg (pfile, rest_args);
        }
    }
  while (token == 20);
}

