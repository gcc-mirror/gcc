/* Common utility code shared between test cases.  */

#ifndef TEST_HELPERSPP_H
#define TEST_HELPERSPP_H

namespace libgdiagnostics {

inline physical_location
make_range (manager &mgr,
	    file f,
	    line_num_t line_num,
	    column_num_t start_column,
	    column_num_t end_column)
{
  auto loc_start = mgr.new_location_from_file_line_column (f,
							   line_num,
							   start_column);
  auto loc_end = mgr.new_location_from_file_line_column (f,
							 line_num,
							 end_column);
  return mgr.new_location_from_range (loc_start,
				      loc_start,
				      loc_end);
}

} // namespace libgdiagnostics

#endif /* #ifndef TEST_HELPERSPP_H */
