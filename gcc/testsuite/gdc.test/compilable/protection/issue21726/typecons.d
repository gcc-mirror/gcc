module protection.issue21726.typecons;

import protection.issue21726.format : issuePkgSym;
import protection.issue21726.format : protectionPkgSym;
static assert(!__traits(compiles,
                        { import protection.issue21726.format : formatPkgSym; }));
