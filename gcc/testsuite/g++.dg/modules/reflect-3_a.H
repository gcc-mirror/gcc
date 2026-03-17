// PR c++/124200
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodule-header -freflection" }
// { dg-module-cmi {} }

// FIXME: This header unit is only needed because we don't support
// standard library merging with -freflection yet.
#include <meta>
