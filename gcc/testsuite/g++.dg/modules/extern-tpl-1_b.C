// { dg-additional-options {-fmodules-ts -fmodule-mapper=|@g++-mapper-server} }

// Have to textually include, because we currently get confused about
// the explicit instantiations and think they conflict
#include "extern-tpl-1_a.H"

template class TPL<1>;
