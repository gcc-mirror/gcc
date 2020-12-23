// { dg-additional-options {-fmodule-header -W} }

// { dg-module-cmi {} }

#include "tpl-friend-merge-1.h"
extern template class basic_streambuf<char>;
