// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Jul 2003 <nathan@codesourcery.com>

// PR 11282. Infinite loop/memory consumption

struct parameter_struct_t {
  char short_option;
  char *long_option;
};

parameter_struct_t *parameters[] = {
  {
    'f'; // { dg-error "error before" "" }
    "from";
  };
};

