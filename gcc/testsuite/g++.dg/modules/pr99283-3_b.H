// { dg-additional-options {-fmodule-header -fno-module-lazy} }
// { dg-module-cmi {} }

template<typename _CharT>
class basic_string;

typedef basic_string<char> string;

import  "pr99283-3_a.H";
