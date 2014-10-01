
int
phoo();

int
bhar();

#ifndef __has_include_next
#  error "__has_include_next"
#else
#  if __has_include_next("phoobhar.h")
#    include_next "phoobhar.h"
#  else
#    error "__has_include_next(\"phoobhar.h\")"
#  endif
#endif
