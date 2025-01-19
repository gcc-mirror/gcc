/* Common utility code shared between test cases.  */

#ifndef TEST_HELPERS_H
#define TEST_HELPERS_H

const diagnostic_physical_location *
make_range (diagnostic_manager *diag_mgr,
	    const diagnostic_file *file,
	    diagnostic_line_num_t line_num,
	    diagnostic_column_num_t start_column,
	    diagnostic_column_num_t end_column)
{
  const diagnostic_physical_location *loc_start
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     file,
							     line_num,
							     start_column);
  const diagnostic_physical_location *loc_end
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     file,
							     line_num,
							     end_column);
  return diagnostic_manager_new_location_from_range (diag_mgr,
						     loc_start,
						     loc_start,
						     loc_end);
}

/* A begin_test/end_test pair to consolidate the code shared by tests:
   create a diagnostic_manager, a main file, a text sink, and a SARIF sink,
   and clean these up after emitting zero or more diagnostics.  */

static diagnostic_manager *diag_mgr;
static const diagnostic_file *main_file;
static FILE *sarif_outfile;

static void
begin_test (const char *tool_name,
	    const char *sarif_output_name,
	    const char *main_file_name,
	    const char *source_language)
{
  diag_mgr = diagnostic_manager_new ();

  /* We need to set this for generated .sarif files to validate
     against the schema.  */
  diagnostic_manager_set_tool_name (diag_mgr, tool_name);

  main_file = diagnostic_manager_new_file (diag_mgr,
					   main_file_name,
					   source_language);

  diagnostic_manager_add_text_sink (diag_mgr, stderr,
				    DIAGNOSTIC_COLORIZE_IF_TTY);
  sarif_outfile = fopen (sarif_output_name, "w");
  if (sarif_outfile)
    diagnostic_manager_add_sarif_sink (diag_mgr,
				       sarif_outfile,
				       main_file,
				       DIAGNOSTIC_SARIF_VERSION_2_1_0);
}

static int
end_test (void)
{
  diagnostic_manager_release (diag_mgr);
  if (sarif_outfile)
    fclose (sarif_outfile);
  return 0;
}

#endif /* #ifndef TEST_HELPERS_H */
