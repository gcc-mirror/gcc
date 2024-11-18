/* Example of emitting a warning with an execution path.

TODO:

   Intended output is similar to:

   along with the equivalent in SARIF.  */

#include "libdiagnostics.h"
#include "test-helpers.h"

/*
_________111111111122222222223333333333444444444455555555556
123456789012345678901234567890123456789012345678901234567890
begin fake source
PyObject *
make_a_list_of_random_ints_badly(PyObject *self,
				 PyObject *args)
{
  PyObject *list, *item;
  long count, i;

  if (!PyArg_ParseTuple(args, "i", &count)) {
    return NULL;
  }

  list = PyList_New(0);
	
  for (i = 0; i < count; i++) {
    item = PyLong_FromLong(random());
    PyList_Append(list, item);
  }
  
  return list;
}
end fake source
*/
const int final_line_num = __LINE__ - 4; /* line of "return list;" */

/* begin line consts */
const int line_num_call_to_PyList_New = final_line_num - 7;
const int line_num_for_loop = final_line_num - 5;
const int line_num_call_to_PyList_Append = final_line_num - 3;
/* end line consts */

int
main ()
{
  begin_test ("test-warning-with-path.c.exe",
	      "test-warning-with-path.c.sarif",
	      __FILE__, "c");

  /* begin full example */
  /* begin create phys locs */
  const diagnostic_physical_location *loc_call_to_PyList_New
    = make_range (diag_mgr, main_file, line_num_call_to_PyList_New, 10, 22);
  const diagnostic_physical_location *loc_for_cond
    = make_range (diag_mgr, main_file, line_num_for_loop, 15, 23);
  const diagnostic_physical_location *loc_call_to_PyList_Append
    = make_range (diag_mgr, main_file, line_num_call_to_PyList_Append, 5, 29);
  /* end create phys locs */

  /* begin create logical locs */
  const char *funcname = "make_a_list_of_random_ints_badly";
  const diagnostic_logical_location *logical_loc
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
					       NULL, /* parent */
					       funcname,
					       funcname,
					       funcname);
  /* end create logical locs */

  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_WARNING);
  diagnostic_set_location (d, loc_call_to_PyList_Append);
  diagnostic_set_logical_location (d, logical_loc);

  /* begin path creation */
  diagnostic_execution_path *path = diagnostic_add_execution_path (d);
  
  diagnostic_event_id alloc_event_id
    = diagnostic_execution_path_add_event (path,
					   loc_call_to_PyList_New,
					   logical_loc, 0,
					   "when %qs fails, returning NULL",
					   "PyList_New");
  diagnostic_execution_path_add_event (path,
				       loc_for_cond,
				       logical_loc, 0,
				       "when %qs", "i < count");
  diagnostic_execution_path_add_event (path,
				       loc_call_to_PyList_Append,
				       logical_loc, 0,
				       "when calling %qs, passing NULL from %@ as argument %i",
				       "PyList_Append", &alloc_event_id, 1);
  /* end path creation */

  diagnostic_finish (d,
		     "passing NULL as argument %i to %qs"
		     " which requires a non-NULL parameter",
		     1, "PyList_Append");
  /* end full example */
  
  return end_test ();
};

/* Check the output from the text sink.
   { dg-begin-multiline-output "" }
In function 'make_a_list_of_random_ints_badly':
   { dg-end-multiline-output "" }
   { dg-regexp "\[^\n\r\]+test-warning-with-path.c:31:5: warning: passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter" }
   { dg-begin-multiline-output "" }
   31 |     PyList_Append(list, item);
      |     ^~~~~~~~~~~~~~~~~~~~~~~~~
  'make_a_list_of_random_ints_badly': events 1-3
   27 |   list = PyList_New(0);
      |          ^~~~~~~~~~~~~
      |          |
      |          (1) when 'PyList_New' fails, returning NULL
   28 | 
   29 |   for (i = 0; i < count; i++) {
      |               ~~~~~~~~~
      |               |
      |               (2) when 'i < count'
   30 |     item = PyLong_FromLong(random());
   31 |     PyList_Append(list, item);
      |     ~~~~~~~~~~~~~~~~~~~~~~~~~
      |     |
      |     (3) when calling 'PyList_Append', passing NULL from (1) as argument 1
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-warning-with-path.c "test-warning-with-path-c.py" } } */
