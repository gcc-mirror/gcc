#  Copyright (C) 2003,2004,2005,2006,2007,2008, 2010, 2011
#  Free Software Foundation, Inc.
#  Contributed by Kelley Cook, June 2004.
#  Original code from Neil Booth, May 2003.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Read in the option records generated from opt-gather.awk.

BEGIN {
	n_opts = 0
	n_langs = 0
	n_target_save = 0
	n_extra_vars = 0
	n_extra_target_vars = 0
	n_extra_masks = 0
	n_extra_c_includes = 0
	n_extra_h_includes = 0
	n_enums = 0
	have_save = 0;
	quote = "\042"
	comma = ","
	FS=SUBSEP
	# Default the name of header created from opth-gen.awk to options.h
	if (header_name == "") header_name="options.h"
}

# Collect the text and flags of each option into an array
	{
		if ($1 == "Language") {
			langs[n_langs] = $2
			n_langs++;
		}
		else if ($1 == "TargetSave") {
			# Make sure the declarations are put in source order
			target_save_decl[n_target_save] = $2
			n_target_save++
		}
		else if ($1 == "Variable") {
			extra_vars[n_extra_vars] = $2
			n_extra_vars++
			name = host_wide_int_var_name($2)
			if (name != "")
				host_wide_int[name] = "yes"
		}
		else if ($1 == "TargetVariable") {
			# Combination of TargetSave and Variable
			extra_vars[n_extra_vars] = $2
			n_extra_vars++

			var = $2
			sub(" *=.*", "", var)
			orig_var = var
			name = var
			type = var
			sub("^.*[ *]", "", name)
			sub(" *" name "$", "", type)
			target_save_decl[n_target_save] = type " x_" name
			n_target_save++

			extra_target_vars[n_extra_target_vars] = name
			n_extra_target_vars++
		}
		else if ($1 == "HeaderInclude") {
			extra_h_includes[n_extra_h_includes++] = $2;
		}
		else if ($1 == "SourceInclude")  {
			extra_c_includes[n_extra_c_includes++] = $2;
		}
		else if ($1 == "Enum") {
			props = $2
			name = opt_args("Name", props)
			type = opt_args("Type", props)
			unknown_error = opt_args("UnknownError", props)
			enum_names[n_enums] = name
			enum_type[name] = type
			enum_index[name] = n_enums
			enum_unknown_error[name] = unknown_error
			enum_help[name] = $3
			n_enums++
		}
		else if ($1 == "EnumValue")  {
			props = $2
			enum_name = opt_args("Enum", props)
			string = opt_args("String", props)
			value = opt_args("Value", props)
			val_flags = "0"
			val_flags = val_flags \
			  test_flag("Canonical", props, "| CL_ENUM_CANONICAL") \
			  test_flag("DriverOnly", props, "| CL_ENUM_DRIVER_ONLY")
			enum_data[enum_name] = enum_data[enum_name] \
			  "  { " quote string quote ", " value ", " val_flags \
			  " },\n"
		}
		else {
			name = opt_args("Mask", $1)
			if (name == "") {
				opts[n_opts]  = $1
				flags[n_opts] = $2
				help[n_opts]  = $3
				for (i = 4; i <= NF; i++)
					help[n_opts] = help[n_opts] " " $i
				n_opts++;
			}
			else {
				extra_masks[n_extra_masks++] = name
			}
		}
	}

