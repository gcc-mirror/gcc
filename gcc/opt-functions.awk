#  Copyright (C) 2003, 2004, 2007, 2008, 2009, 2010
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

# Some common subroutines for use by opt[ch]-gen.awk.

# Define some helpful character classes, for portability.
BEGIN {
	lower = "abcdefghijklmnopqrstuvwxyz"
	upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	digit = "0123456789"
	alnum = lower "" upper "" digit
}

# Return nonzero if FLAGS contains a flag matching REGEX.
function flag_set_p(regex, flags)
{
	return (" " flags " ") ~ (" " regex " ")
}

# Return STRING if FLAGS contains a flag matching regexp REGEX,
# otherwise return the empty string.
function test_flag(regex, flags, string)
{
	if (flag_set_p(regex, flags))
		return string
	return ""
}

# If FLAGS contains a "NAME(...argument...)" flag, return the value
# of the argument.  Return the empty string otherwise.
function opt_args(name, flags)
{
	flags = " " flags
	if (flags !~ " " name "\\(")
		return ""
	sub(".* " name "\\(", "", flags)
	if (flags ~ "^{")
	{
		sub ("^{", "", flags)
		sub("}\\).*", "", flags)
	}
	else
		sub("\\).*", "", flags)

	return flags
}

# Return the Nth comma-separated element of S.  Return the empty string
# if S does not contain N elements.
function nth_arg(n, s)
{
	while (n-- > 0) {
		if (s !~ ",")
			return ""
		sub("[^,]*, *", "", s)
	}
	sub(",.*", "", s)
	return s
}

# Return a bitmask of CL_* values for option flags FLAGS.
function switch_flags (flags)
{
	result = "0"
	for (j = 0; j < n_langs; j++) {
		regex = langs[j]
		gsub ( "\\+", "\\+", regex )
		result = result test_flag(regex, flags, " | " macros[j])
	}
	result = result \
	  test_flag("Common", flags, " | CL_COMMON") \
	  test_flag("Target", flags, " | CL_TARGET") \
	  test_flag("Driver", flags, " | CL_DRIVER") \
	  test_flag("RejectDriver", flags, " | CL_REJECT_DRIVER") \
	  test_flag("NoDriverArg", flags, " | CL_NO_DRIVER_ARG") \
	  test_flag("SeparateAlias", flags, " | CL_SEPARATE_ALIAS") \
	  test_flag("Save", flags, " | CL_SAVE") \
	  test_flag("Joined", flags, " | CL_JOINED") \
	  test_flag("JoinedOrMissing", flags, " | CL_JOINED | CL_MISSING_OK") \
	  test_flag("Separate", flags, " | CL_SEPARATE") \
	  test_flag("RejectNegative", flags, " | CL_REJECT_NEGATIVE") \
	  test_flag("UInteger", flags, " | CL_UINTEGER") \
	  test_flag("Undocumented", flags,  " | CL_UNDOCUMENTED") \
	  test_flag("Warning", flags,  " | CL_WARNING") \
	  test_flag("Optimization", flags,  " | CL_OPTIMIZATION") \
	  test_flag("Report", flags, " | CL_REPORT")
	sep_args = opt_args("Args", flags)
	if (sep_args != "") {
		sep_args--
		result = result " | (" sep_args \
		    " << CL_SEPARATE_NARGS_SHIFT)"
	}
	sub( "^0 \\| ", "", result )
	return result
}

# If FLAGS includes a Var flag, return the name of the variable it specifies.
# Return the empty string otherwise.
function var_name(flags)
{
	return nth_arg(0, opt_args("Var", flags))
}

# Return true if the option described by FLAGS has a globally-visible state.
function global_state_p(flags)
{
	return (var_name(flags) != "" \
		|| opt_args("Mask", flags) != "" \
		|| opt_args("InverseMask", flags) != "")
}

# Return true if the option described by FLAGS must have some state
# associated with it.
function needs_state_p(flags)
{
	return (flag_set_p("Target", flags) \
		&& !flag_set_p("Alias.*", flags) \
		&& !flag_set_p("Ignore", flags))
}

# If FLAGS describes an option that needs state without a public
# variable name, return the name of that field, minus the initial
# "x_", otherwise return "".  NAME is the name of the option.
function static_var(name, flags)
{
	if (global_state_p(flags) || !needs_state_p(flags))
		return ""
	gsub ("[^" alnum "]", "_", name)
	return "VAR_" name
}

# Return the type of variable that should be associated with the given flags.
function var_type(flags)
{
	if (flag_set_p("Defer", flags))
		return "void *"
	else if (flag_set_p("Enum.*", flags)) {
		en = opt_args("Enum", flags);
		return enum_type[en] " "
	}
	else if (!flag_set_p("Joined.*", flags) && !flag_set_p("Separate", flags))
		return "int "
	else if (flag_set_p("UInteger", flags))
		return "int "
	else
		return "const char *"
}

# Return the type of variable that should be associated with the given flags
# for use within a structure.  Simple variables are changed to signed char
# type instead of int to save space.
function var_type_struct(flags)
{
	if (flag_set_p("UInteger", flags))
		return "int "
	else if (!flag_set_p("Joined.*", flags) && !flag_set_p("Separate", flags)) {
		if (flag_set_p(".*Mask.*", flags))
			return "int "
		else
			return "signed char "
	}
	else
		return "const char *"
}

# Given that an option has flags FLAGS, return an initializer for the
# "var_enum", "var_type" and "var_value" fields of its cl_options[] entry.
function var_set(flags)
{
	if (flag_set_p("Defer", flags))
		return "0, CLVC_DEFER, 0"
	s = nth_arg(1, opt_args("Var", flags))
	if (s != "")
		return "0, CLVC_EQUAL, " s
	s = opt_args("Mask", flags);
	if (s != "") {
		vn = var_name(flags);
		if (vn)
			return "0, CLVC_BIT_SET, OPTION_MASK_" s
		else
			return "0, CLVC_BIT_SET, MASK_" s
	}
	s = nth_arg(0, opt_args("InverseMask", flags));
	if (s != "") {
		vn = var_name(flags);
		if (vn)
			return "0, CLVC_BIT_CLEAR, OPTION_MASK_" s
		else
			return "0, CLVC_BIT_CLEAR, MASK_" s
	}
	if (flag_set_p("Enum.*", flags)) {
		en = opt_args("Enum", flags);
		return enum_index[en] ", CLVC_ENUM, 0"
	}
	if (var_type(flags) == "const char *")
		return "0, CLVC_STRING, 0"
	return "0, CLVC_BOOLEAN, 0"
}

# Given that an option called NAME has flags FLAGS, return an initializer
# for the "flag_var" field of its cl_options[] entry.
function var_ref(name, flags)
{
	name = var_name(flags) static_var(name, flags)
	if (name != "")
		return "offsetof (struct gcc_options, x_" name ")"
	if (opt_args("Mask", flags) != "")
		return "offsetof (struct gcc_options, x_target_flags)"
	if (opt_args("InverseMask", flags) != "")
		return "offsetof (struct gcc_options, x_target_flags)"
	return "-1"
}

# Given the option called NAME return a sanitized version of its name.
function opt_sanitized_name(name)
{
	gsub ("[^" alnum "]", "_", name)
	return name
}

# Given the option called NAME return the appropriate enum for it.
function opt_enum(name)
{
	return "OPT_" opt_sanitized_name(name)
}
