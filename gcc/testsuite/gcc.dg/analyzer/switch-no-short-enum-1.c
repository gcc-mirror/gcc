/* { dg-do compile } */
/* { dg-additional-options "-fno-short-enums" } */
/* { dg-skip-if "default" { ! short_enums } } */

#include "analyzer-decls.h"

/* Verify the handling of "switch (enum_value)".  */

enum e
{
 E_VAL0,
 E_VAL1,
 E_VAL2
};

/* Verify that we assume that "switch (enum)" doesn't follow implicit
   "default" if all enum values have cases  */

int test_all_values_covered_implicit_default_1 (enum e x)
{
  switch (x)
    {
    case E_VAL0:
      return 1066;
    case E_VAL1:
      return 1776;
    case E_VAL2:
      return 1945;
    }
  __analyzer_dump_path (); /* { dg-bogus "path" } */
}

int test_all_values_covered_implicit_default_2 (enum e x)
{
  int result;
  switch (x)
    {
    case E_VAL0:
      result = 1066;
      break;
    case E_VAL1:
      result = 1776;
      break;
    case E_VAL2:
      result = 1945;
      break;
    }
  return result; /* { dg-bogus "uninitialized" } */
}

/* Verify that we consider paths that use the implicit default when not
   all enum values are covered by cases.  */

int test_missing_values_implicit_default_1 (enum e x)
{
  switch (x) /* { dg-message "following 'default:' branch" } */
    {
    case E_VAL0:
      return 1066;
    case E_VAL1:
      return 1776;
    }
  __analyzer_dump_path (); /* { dg-message "path" } */
  return 0;
}

int test_missing_values_implicit_default_2 (enum e x)
{
  int result;
  switch (x) /* { dg-message "following 'default:' branch" } */
    {
    case E_VAL0:
      result = 1066;
      break;
    case E_VAL1:
      result = 1776;
      break;
    }
  return result; /* { dg-warning "uninitialized" } */
}

/* Verify that explicit "default" isn't rejected.  */

int test_all_values_covered_explicit_default_1 (enum e x)
{
  switch (x)
    {
    case E_VAL0:
      return 1066;
    case E_VAL1:
      return 1776;
    case E_VAL2:
      return 1945;
    default:
      __analyzer_dump_path (); /* { dg-message "path" } */
      return 0;
    }
}

int test_missing_values_explicit_default_1 (enum e x)
{
  switch (x)
    {
    default:
    case E_VAL0:
      return 1066;
    case E_VAL1:
      return 1776;
    }
  __analyzer_dump_path (); /* { dg-bogus "path" } */
  return 0;
}

int test_missing_values_explicit_default_2 (enum e x)
{
  switch (x)
    {
    case E_VAL0:
      return 1066;
    case E_VAL1:
      return 1776;
    default:
      __analyzer_dump_path (); /* { dg-message "path" } */
      return 1945;
    }
  __analyzer_dump_path (); /* { dg-bogus "path" } */
  return 0;
}

int test_just_default (enum e x)
{
  switch (x)
    {
    default:
      __analyzer_dump_path (); /* { dg-message "path" } */
      return 42;
    }
  __analyzer_dump_path (); /* { dg-bogus "path" } */
  return 0;  
}

