/* Copyright (C) 2000  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

#define foo/**/1
#if foo != 1
# error "foo not properly defined"
#endif
