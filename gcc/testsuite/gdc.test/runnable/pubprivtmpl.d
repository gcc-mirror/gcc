// EXTRA_SOURCES: imports/pubprivtmpla.d

module pubprivtmpl;

// Idiom: public alias to private template
// This idiom was discovered while refactoring access.d.  The idiom was not being used in any DLang repository
// but was being used by a few projects in the D ecosystem.  It is unkown at this time if this idiom is permitted
// by design or by accident.  This test was added to DMD to prevent regressions in those projects that utilize this
// idiom.  See also:
// https://issues.dlang.org/show_bug.cgi?id=4533
// https://issues.dlang.org/show_bug.cgi?id=11173

import pubprivtmpla;

void main()
{
   auto s = S();
   auto v = s.get();
   assert(v == 42);
}
