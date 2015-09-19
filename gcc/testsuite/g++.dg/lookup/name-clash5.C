// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// C++ Standard, 3.3, clause 4:
// "[Note: a namespace name or a class template name must be unique in its
// declarative region (7.3.2, clause 14). ]"

namespace N // { dg-message "previous declaration" }
{
}

class N; // { dg-error "redeclared" }
