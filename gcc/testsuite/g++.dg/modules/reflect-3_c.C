// PR c++/124200
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection" }

import M;

// From this context we should see ns::bar but not ns::foo.
static_assert(num_members<^^ns>() == 1);
