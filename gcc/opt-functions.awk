#  Copyright (C) 2003,2004 Free Software Foundation, Inc.
#  Contributed by Kelley Cook, June 2004.
#  Original code from Neil Booth, May 2003.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# Some common subroutines for use by opt[ch]-gen.awk.

function switch_flags (flags)
{
	flags = " " flags " "
	result = "0"
	for (j = 0; j < n_langs; j++) {
		regex = " " langs[j] " "
		gsub ( "\\+", "\\+", regex )
		if (flags ~ regex)
			result = result " | " macros[j]
	}
	if (flags ~ " Common ") result = result " | CL_COMMON"
	if (flags ~ " Joined ") result = result " | CL_JOINED"
	if (flags ~ " JoinedOrMissing ") \
	    result = result " | CL_JOINED | CL_MISSING_OK"
	if (flags ~ " Separate ") result = result " | CL_SEPARATE"
	if (flags ~ " RejectNegative ") result = result " | CL_REJECT_NEGATIVE"
	if (flags ~ " UInteger ") result = result " | CL_UINTEGER"
	if (flags ~ " Undocumented ") result = result " | CL_UNDOCUMENTED"
	if (flags ~ " Report ") result = result " | CL_REPORT"
	sub( "^0 \\| ", "", result )
	return result
}

function var_args(flags)
{
	if (flags !~ "Var\\(")
	    return ""
	sub(".*Var\\(", "", flags)
	sub("\\).*", "", flags)

	return flags
}
function var_name(flags)
{
	s = var_args(flags)
	if (s == "")
		return "";
	sub( ",.*", "", s)
	return s
}
function var_set(flags)
{
	s = var_args(flags)
	if (s !~ ",")
		return "0, 0"
	sub( "[^,]*,", "", s)
	return "1, " s
}
function var_ref(flags)
{
	name = var_name(flags)
	if (name == "")
		return "0"
	else
		return "&" name
}
