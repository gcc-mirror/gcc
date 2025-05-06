/* Example of nested logical locations, based on the JSON example in
   SARIF v2.1.0, 3.33.7 "kind" property;
   though see https://github.com/oasis-tcs/sarif-spec/issues/670

   Intended output is similar to:

In JSON value '/orders/0/productIds/1':
PATH/test-nested-logical-locations-json.c:28:32: warning: product ID is blank
   28 |       "productIds": [ "A-101", "", "A-223" ],
      |                                ^~
In JSON property '/orders/0/total':
PATH/test-nested-logical-locations-json.c:29:16: warning: value is negative
   29 |       "total": "-3.25"
      |                ^~~~~~~

   along with the equivalent in SARIF, capturing JSON structure
   as nested logical locations.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/* Placeholder source:
_________1111111111222222222233333333334444444444
1234567890123456789012345678901234567890123456789
{
  "orders": [
    {
      "productIds": [ "A-101", "", "A-223" ],
      "total": "-3.25"
    }
  ]
}
*/
const int start_line_num = __LINE__ - 9;
const int line_num_of_product_ids = start_line_num + 3;
const int line_num_of_total = line_num_of_product_ids + 1;

#include <assert.h>

int
main ()
{
  begin_test ("test-nested-logical-locations-json.c.exe",
	      "test-nested-logical-locations-json.c.sarif",
	      __FILE__, "c");

  /* Create tree of logical locations.  */
  /* begin quoted source */
  const diagnostic_logical_location *logical_loc_orders_arr
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_ARRAY,
					       NULL, /* parent */
					       "orders",
					       "/orders",
					       NULL);
  const diagnostic_logical_location *logical_loc_order_0
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_OBJECT,
					       logical_loc_orders_arr, /* parent */
					       "0",
					       "/orders/0",
					       NULL);
  const diagnostic_logical_location *logical_loc_product_ids
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_ARRAY,
					       logical_loc_order_0, /* parent */
					       "productIds",
					       "/orders/0/productIds",
					       NULL);
  const diagnostic_logical_location *logical_loc_element_1
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_VALUE,
					       logical_loc_product_ids, /* parent */
					       "1",
					       "/orders/0/productIds/1",
					       NULL);
  const diagnostic_logical_location *logical_loc_total
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROPERTY,
					       logical_loc_order_0, /* parent */
					       "total",
					       "/orders/0/total",
					       NULL);
  /* end quoted source */

  {
    const int line_num = line_num_of_product_ids;
    const diagnostic_physical_location *loc_start
      = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							       main_file,
							       line_num,
							       32);
    const diagnostic_physical_location *loc_end
      = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							       main_file,
							       line_num,
							       33);
    const diagnostic_physical_location *loc_range
      = diagnostic_manager_new_location_from_range (diag_mgr,
						    loc_start,
						    loc_start,
						    loc_end);

    diagnostic *d = diagnostic_begin (diag_mgr,
				      DIAGNOSTIC_LEVEL_WARNING);
    diagnostic_set_location (d, loc_range);

    diagnostic_set_logical_location (d, logical_loc_element_1);

    diagnostic_finish (d, "product ID is blank");
  }
  {
    const int line_num = line_num_of_total;
    const diagnostic_physical_location *loc_start
      = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							       main_file,
							       line_num,
							       16);
    const diagnostic_physical_location *loc_end
      = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							       main_file,
							       line_num,
							       22);
    const diagnostic_physical_location *loc_range
      = diagnostic_manager_new_location_from_range (diag_mgr,
						    loc_start,
						    loc_start,
						    loc_end);

    diagnostic *d = diagnostic_begin (diag_mgr,
				      DIAGNOSTIC_LEVEL_WARNING);
    diagnostic_set_location (d, loc_range);

    diagnostic_set_logical_location (d, logical_loc_total);

    diagnostic_finish (d, "value is negative");
  }

  return end_test ();
}

/* Check the output from the text sink.  */
/* { dg-begin-multiline-output "" }
In JSON value '/orders/0/productIds/1':
   { dg-end-multiline-output "" } */
/* { dg-regexp "\[^\n\r\]+test-nested-logical-locations-json.c:28:32: warning: product ID is blank" } */
/* { dg-begin-multiline-output "" }
   28 |       "productIds": [ "A-101", "", "A-223" ],
      |                                ^~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
In JSON property '/orders/0/total':
   { dg-end-multiline-output "" } */
/* { dg-regexp "\[^\n\r\]+test-nested-logical-locations-json.c:29:16: warning: value is negative" } */
/* { dg-begin-multiline-output "" }
   29 |       "total": "-3.25"
      |                ^~~~~~~
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-nested-logical-locations-json.c "test-nested-logical-locations-json-c.py" } } */
