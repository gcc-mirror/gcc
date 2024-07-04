#include "analyzer-decls.h"

enum color
{
 RED,
 GREEN,
 BLUE
};

enum fruit
{
 APPLE,
 BANANA
};

int test_wrong_enum (enum color x)
{
  switch (x)
    {
    case APPLE:
      return 1066;
    case BANANA:
      return 1776;
    }
  __analyzer_dump_path (); /* { dg-message "path" } */
  return 0;
}
