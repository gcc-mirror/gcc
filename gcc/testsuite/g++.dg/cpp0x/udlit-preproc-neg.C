// { dg-do compile { target c++11 } }

int
operator"" _badpreproc(const char *str)
{ return 0; }

#if 123_badpreproc  //  { dg-error "user-defined literal in preprocessor expression" }
#  error ("user-defined literal in preprocessor expression")  //  { dg-error "user-defined literal in preprocessor expression" }
#endif
