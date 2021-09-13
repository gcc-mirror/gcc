// { dg-additional-options {-fmodule-header --param ggc-min-expand=0 --param ggc-min-heapsize=0} }
// { dg-module-cmi {} }

import "pr99423_a.H";

pair<bool> boom();
