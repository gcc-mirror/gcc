/* Subroutines used for reading MCU data on TI MSP430 processors.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
   Contributed by Jozef Lawrynowicz  <jozef.l@mittosystems.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "memmodel.h"
#include "diagnostic-core.h"
#include "langhooks.h"
#include "builtins.h"
#include "intl.h"
#include "msp430-devices.h"

struct t_msp430_mcu_data extracted_mcu_data;
/* Initialized at the bottom of this file.  */
extern struct t_msp430_mcu_data hard_msp430_mcu_data[605];

/* Set to the full path to devices.csv if it is found by searching the -I and
   -L paths.  */
char *derived_devices_csv_loc = NULL;

/* This is to canonicalize the directory separators in the path.
   On Windows we could have a mix of '/' and '\' in the path.  */
static void
canonicalize_path_dirsep (char **path)
{
  char *t_path = *path;
  int len = strlen (t_path);
  int i;
  for (i = 0; i < len; i++)
    if (IS_DIR_SEPARATOR (t_path[i]))
      t_path[i] = DIR_SEPARATOR;
}

/* This function returns the enclosing directory of PATH.
   It is inconsequential whether PATH ends in a dirsep or not.
   It modifies the string pointed to by PATH.  */
char *
msp430_dirname (char *path)
{
  int last_elem = strlen (path) - 1;
  int i = last_elem - (IS_DIR_SEPARATOR (path[last_elem]) ? 1 : 0);
  for (; i >= 0; i--)
    {
      if (IS_DIR_SEPARATOR (path[i]))
	{
	  path[i] = '\0';
	  return path;
	}
    }
  return path;
}

/* We need to support both the msp430-elf and msp430-elfbare target aliases.
   gcc/config/msp430/t-msp430 will define TARGET_SUBDIR to the target_subdir
   Makefile variable, which will evaluate to the correct subdirectory that
   needs to be searched for devices.csv.  */
#ifndef TARGET_SUBDIR
#define TARGET_SUBDIR msp430-elf
#endif

#define _MSPMKSTR(x) __MSPMKSTR(x)
#define __MSPMKSTR(x) #x

/* devices.csv path from the toolchain root.  */
static const char rest_of_devices_path[] =
  "/" _MSPMKSTR (TARGET_SUBDIR) "/include/devices/";

#undef _MSPMKSTR
#undef __MSPMKSTR

/* "The default value of GCC_EXEC_PREFIX is prefix/lib/gcc". Strip lib/gcc
   from GCC_EXEC_PREFIX to get the path to the installed toolchain.  */
static void
extract_devices_dir_from_exec_prefix (char **devices_loc)
{
  const char *temp;
  char *gcc_exec_prefix = *devices_loc;
  int len = strlen (gcc_exec_prefix);

  /* Copied from gcc.cc.  */
  if (len > (int) sizeof ("/lib/gcc/") - 1
      && (IS_DIR_SEPARATOR (gcc_exec_prefix[len-1])))
    {
      temp = gcc_exec_prefix + len - sizeof ("/lib/gcc/") + 1;
      if (IS_DIR_SEPARATOR (*temp)
	  && filename_ncmp (temp + 1, "lib", 3) == 0
	  && IS_DIR_SEPARATOR (temp[4])
	  && filename_ncmp (temp + 5, "gcc", 3) == 0)
	{
	  len -= sizeof ("/lib/gcc/") - 1;
	  /* Keep the '/' from the beginning of /lib/gcc.  */
	  gcc_exec_prefix[len + 1] = (char) 0;
	  *devices_loc = concat (gcc_exec_prefix, rest_of_devices_path, NULL);
	  return;
	}
    }
}

/* Given the path to the GCC executable, return the path to the installed
   device data in "$TOOLCHAIN_ROOT/msp430-elf/include/devices".
   Assumes the GCC executable is in "$TOOLCHAIN_ROOT/<somedir>/".  */
static void
extract_devices_dir_from_collect_gcc (char **devices_loc)
{
  char *t_devices_loc = *devices_loc;
  /* Go up a directory to the toolchain root.  */
  t_devices_loc = msp430_dirname (msp430_dirname (t_devices_loc));
  t_devices_loc = concat (t_devices_loc, rest_of_devices_path, NULL);
  *devices_loc = t_devices_loc;
}

/* The path to the MSP430-GCC support files can be specified with the
   environment variable "MSP430_GCC_INCLUDE_DIR", or installed into the
   toolchain in the msp430-elf/include/devices subdirectory.
   We use the GCC_EXEC_PREFIX or COLLECT_GCC environment variables as a starting
   point for the location of the toolchain, and work out the path to the
   installed device data from there.
   Return 0 and set LOCAL_DEVICES_CSV_LOC if we find devices.csv.  Return 1
   if devices.csv wasn't found.  */
int
msp430_check_env_var_for_devices (char **local_devices_csv_loc)
{
  const int num_vars = 3;
  const char dirsep[2] = { DIR_SEPARATOR, 0 };
  /* Both GCC_EXEC_PREFIX and COLLECT_GCC should always be set to the format we
     expect, as they are required for correct operation of the toolchain.
     So if they are wrong the user will probably have bigger problems.
     GCC_EXEC_PREFIX is only defined in the driver, whilst COLLECT_GCC is only
     defined in the compiler proper, so we need both.  */
  const char *env_vars[num_vars] = {
      "MSP430_GCC_INCLUDE_DIR", "GCC_EXEC_PREFIX", "COLLECT_GCC" };
  enum msp430_include_vars {
      MSP430_GCC_INCLUDE_DIR,
      GCC_EXEC_PREFIX,
      COLLECT_GCC
  };
  FILE *devices_csv_file = NULL;
  int i;

  for (i = MSP430_GCC_INCLUDE_DIR; i <= COLLECT_GCC; i++)
    {
      char *t_devices_loc;
      char *val = getenv (env_vars[i]);
      if (val == NULL)
	continue;
      t_devices_loc = xstrdup (val);

      if (i == MSP430_GCC_INCLUDE_DIR)
	{
	  if (!IS_DIR_SEPARATOR (t_devices_loc[strlen (t_devices_loc) - 1]))
	    t_devices_loc = concat (t_devices_loc, dirsep, NULL);
	}
      else if (i == GCC_EXEC_PREFIX)
	extract_devices_dir_from_exec_prefix (&t_devices_loc);
      else if (i == COLLECT_GCC)
	extract_devices_dir_from_collect_gcc (&t_devices_loc);

      t_devices_loc = concat (t_devices_loc, "devices.csv", NULL);
      devices_csv_file = fopen (t_devices_loc,  "r");
      if (devices_csv_file != NULL)
	{
	  fclose (devices_csv_file);
	  *local_devices_csv_loc = t_devices_loc;
	  canonicalize_path_dirsep (local_devices_csv_loc);
	  return 0;
	}
    }
  return 1;
}

/* Spec function which searches the paths passed to the -I and -L options for
   the "devices.csv" file.  If it is found then the -mdevices-csv-loc option is
   placed on the command line so the compiler knows the location of the
   file.  */
const char *
msp430_check_path_for_devices (int argc, const char **argv)
{
  const char dirsep[2] = { DIR_SEPARATOR, 0 };
  FILE * devices_file = NULL;
  char * local_devices_csv_loc = NULL;
  int i;
  /* msp430_devices_csv_loc is set by -mdevices-csv-loc, derived_devices_csv_loc
     is set by this function only.  */
  if (msp430_devices_csv_loc || derived_devices_csv_loc)
    return NULL;
  for (i = 0; i < argc; i++)
    {
      char *inc_path = xstrdup (argv[i]);
      canonicalize_path_dirsep (&inc_path);
      if (!IS_DIR_SEPARATOR (inc_path[strlen (inc_path) - 1]))
	inc_path = concat (inc_path, dirsep, NULL);
      local_devices_csv_loc = concat (inc_path, "devices.csv", NULL);
      devices_file = fopen (local_devices_csv_loc, "r");
      if (devices_file != NULL)
	{
	  fclose (devices_file);
	  derived_devices_csv_loc = local_devices_csv_loc;
	  return concat ("-mdevices-csv-loc=", local_devices_csv_loc, NULL);
	}
    }
  return NULL;
}

/* Search the devices.csv file for the given MCU name, and load the device
   data into extracted_mcu_data.
   Return 1 if MCU wasn't found in devices.csv, or the data couldn't be loaded
   into extracted_mcu_data.
   devices.csv has a specific format.  There is a row for column headings which
   begins with "# Device Name".  The column numbers for CPU_TYPE (MSP430 ISA)
   and MPY_TYPE (hwmult support) are extracted from this row and used later to
   extract the ISA and hwmult supported for the given device.
   The rows containing the MCU data are expected to begin immediately after the
   column headings.  */
static int
parse_devices_csv_1 (const char * real_devices_csv_loc, const char * mcu_name)
{
  FILE * devices_file = fopen (real_devices_csv_loc, "r");
  /* Some devices have a large number of errata, which means that MPY_TYPE
     isn't found until the ~100th character in the line.  line_buf_size is set
     to 200 to account for further possible additions to errata.  */
  const size_t line_buf_size = 200;
  char line[line_buf_size];
  char * res;
  bool found_headings = false;
  bool found_mcu = false;
  int cpu_type = -1;
  int mpy_type = -1;
  int cpu_type_column = -1;
  int mpy_type_column = -1;
  const char * device_name_heading = "# Device Name";
  const char * cpu_type_heading = "CPU_TYPE";
  const char * mpy_type_heading = "MPY_TYPE";
  /* devices_file should never be NULL at this stage.  */
  if (devices_file == NULL)
    {
      if (msp430_warn_devices_csv)
	warning (0, "unexpected error opening %<devices.csv%>");
      return 1;
    }
  while (1)
    {
      res = fgets (line, line_buf_size, devices_file);
      if (res == NULL)
	{
	  /* The device has not been found in devices.csv.  Don't warn now in
	     case it is in the hard-coded data.  We will warn later if the
	     device was not found in the hard-coded data either.  */
	  goto end;
	}
      else if (!found_headings
	       && strncmp (line, device_name_heading,
			   strlen (device_name_heading)) == 0)
	{
	  int curr_column = 0;
	  char * heading = strtok (line, ",");
	  found_headings = true;
	  /* Find which column MPY_TYPE and CPU_TYPE are in.  */
	  while (heading != NULL)
	    {
	      if (strncmp (heading, cpu_type_heading,
			   strlen (cpu_type_heading)) == 0)
		cpu_type_column = curr_column;
	      else if (strncmp (heading, mpy_type_heading,
				strlen (mpy_type_heading)) == 0)
		mpy_type_column = curr_column;
	      if (cpu_type_column != -1 && mpy_type_column != -1)
		break;
	      heading = strtok (NULL, ",");
	      curr_column++;
	    }
	  if (cpu_type_column == -1 || mpy_type_column == -1)
	    {
	      if (msp430_warn_devices_csv)
		{
		  if (cpu_type_column == -1 && mpy_type_column != -1)
		    warning (0, "%<CPU_TYPE%> column heading is missing from "
			     "%<devices.csv%>");
		  else if (mpy_type_column == -1 && cpu_type_column != -1)
		    warning (0, "%<MPY_TYPE%> column heading is missing from "
			     "%<devices.csv%>");
		  else
		    warning (0, "%<CPU_TYPE%> and %<MPY_TYPE%> column headings "
			     "are missing from %<devices.csv%>");
		}
	      goto end;
	    }
	}
      else if (strncasecmp (line, mcu_name, strlen (mcu_name)) == 0
	       && *(line + strlen (mcu_name)) == ',')
	{
	  if (!found_headings)
	    {
	      if (msp430_warn_devices_csv)
		warning (0, "format of column headings in %<devices.csv%> "
			 "is incorrect");
	      goto end;
	    }
	  char * val = strtok (line, ",");
	  int final_col_num = ((mpy_type_column > cpu_type_column)
			       ? mpy_type_column : cpu_type_column);
	  int curr_col;
	  bool found_cpu = false;
	  bool found_mpy = false;
	  for (curr_col = 0; curr_col <= final_col_num; curr_col++)
	    {
	      /* Strip any new line characters from the last token.  */
	      if (curr_col == final_col_num && strlen (val) > 1
		  /* ASCII digit 10 == LF, 13 == CR.  */
		  && (val[1] == 10 || val[1] == 13))
		{
		  /* Terminate the string after the first character.  */
		  val[1] = 0;
		}
	      if (curr_col == cpu_type_column)
		{
		  cpu_type = atoi (val);
		  /* Only a single '0', '1' or '2' is accepted.  */
		  if (strlen (val) != 1
		      /* atoi will return 0 if the string passed as an argument
			 is empty or contains only whitespace characters, so we
			 must error if 0 is returned but the first character in
			 the original string is not '0'.  */
		      || (cpu_type == 0 && val[0] != '0')
		      || cpu_type > 2 || cpu_type < 0)
		    {
		      if (msp430_warn_devices_csv)
			warning (0, "invalid %<CPU_TYPE%> value of %qs read "
				 "from %<devices.csv%> for %qs", val, mcu_name);
		      goto end;
		    }
		  extracted_mcu_data.revision = cpu_type;
		  found_cpu = true;
		}
	      else if (curr_col == mpy_type_column)
		{
		  mpy_type = atoi (val);
		  /* Only a single '0', '1', '2', '4' or '8' is accepted.  */
		  if (strlen (val) != 1
		      || (mpy_type == 0 && val[0] != '0')
		      || !(mpy_type == 0
			   || mpy_type == 1
			   || mpy_type == 2
			   || mpy_type == 4
			   || mpy_type == 8))
		    {
		      if (msp430_warn_devices_csv)
			warning (0, "invalid %<MPY_TYPE%> value of %qs read "
				 "from %<devices.csv%> for %qs", val, mcu_name);
		      goto end;
		    }
		  extracted_mcu_data.hwmpy = mpy_type;
		  found_mpy = true;
		}
	      if (found_cpu && found_mpy)
		{
		  extracted_mcu_data.name = mcu_name;
		  found_mcu = true;
		  goto end;
		}
	      val = strtok (NULL, ",");
	    }
	  if (msp430_warn_devices_csv && (cpu_type == -1 || mpy_type == -1))
	    warning (0, "unknown error reading %s from "
		     "%<devices.csv%>",
		     (cpu_type != -1 ? "%<MPY_TYPE%>"
		      : (mpy_type != -1 ? "%<CPU_TYPE%>"
			 : "%<CPU_TYPE%> and %<MPY_TYPE%>")));
	  goto end;
	}
    }
end:
  fclose (devices_file);
  if (!found_mcu)
    return 1;
  return 0;
}

/* Wrapper for the parse_devices_csv_1 work function.
   A return code of 0 indicates that the MCU data has been successfully
   extracted into extracted_mcu_data.
   A return code of 1 indicates that the specified MCU wasn't found in
   devices.csv.
   A return code of 2 indicates that devices.csv wasn't found at all.  */
static int
parse_devices_csv (const char * mcu_name)
{
  /* First check if the path to devices.csv was set by -mdevices-csv-loc.  */
  if (msp430_devices_csv_loc != NULL)
    return parse_devices_csv_1 (msp430_devices_csv_loc, mcu_name);
  /* Otherwise check if the path to devices.csv was found another way.  */
  else if (derived_devices_csv_loc != NULL)
    return parse_devices_csv_1 (derived_devices_csv_loc, mcu_name);
  /* Otherwise we need to use environment variables to try and find it.  */
  if (msp430_check_env_var_for_devices (&derived_devices_csv_loc))
    /* devices.csv was not found.  */
    return 2;
  return parse_devices_csv_1 (derived_devices_csv_loc, mcu_name);
}

/* Main entry point to load the MCU data for the given -mmcu into
   extracted_mcu_data.
   First, the "devices.csv" MCU data file is searched for, if it is found, and
   the MCU has a record in it, then that data is used.
   Otherwise, hard_msp430_mcu_data (initialized at the bottom of this
   file) is searched for the MCU name.
   This function only needs to be executed once, but it can be first called
   from a number of different locations.  */
void
msp430_extract_mcu_data (const char * mcu_name)
{
  static int executed = 0;
  int devices_csv_not_found = 0;
  int i;
  if (mcu_name == NULL || executed == 1)
    return;
  executed = 1;
  /* If parse_devices_csv returns non-zero we need to use the
     hard-coded data.  */
  switch (parse_devices_csv (mcu_name))
    {
    case 0:
      return;
    case 1:
      /* MCU not found in devices.csv.  Warn later if it's not in the
	 hard-coded data either.  */
      break;
    case 2:
      devices_csv_not_found = 1;
      break;
    default:
      gcc_unreachable ();
    }
  for (i = ARRAY_SIZE (hard_msp430_mcu_data); i--;)
    if (strcasecmp (mcu_name, hard_msp430_mcu_data[i].name) == 0)
      {
	extracted_mcu_data = hard_msp430_mcu_data[i];
	break;
      }
  /* Validation checks.  */
  if (extracted_mcu_data.name != NULL)
    {
      switch (extracted_mcu_data.hwmpy)
	{
	case 0:
	case 1:
	case 2:
	case 4:
	case 8: break;
	default:
	  error ("unrecognized %<hwmpy%> field in "
		 "%<hard_msp430_mcu_data[%d]%>: %qd", i,
		 hard_msp430_mcu_data[i].hwmpy);
	  break;
	}
      switch (extracted_mcu_data.revision)
	{
	case 0:
	case 1:
	case 2: break;
	default:
	  error ("unrecognized %<revision%> field in "
		 "%<hard_msp430_mcu_data[%d]%>: %qd", i,
		 hard_msp430_mcu_data[i].revision);
	}
    }
  else if (msp430_warn_devices_csv && devices_csv_not_found)
    warning (0, "could not locate MCU data file %<devices.csv%>");
  else if (msp430_warn_mcu && extracted_mcu_data.name == NULL)
    {
      /* FIXME: We should warn here that the MCU name is unrecognized, but
	 msp430_option_override will warn about an unrecognized MCU as well.
	 The benefit of warning here is that this is code common to both the
	 driver and compiler proper, so a warning will be emitted when
	 assembling/linking via the driver, whilst msp430_option_override will
	 only be called when preprocessing or compiling.  */
    }
}

/* The data in this structure has been extracted from version 1.194 of the
   devices.csv file released by TI in September 2016.  */

struct t_msp430_mcu_data hard_msp430_mcu_data[605] =
  {
    { "cc430f5123",2,8 },
    { "cc430f5125",2,8 },
    { "cc430f5133",2,8 },
    { "cc430f5135",2,8 },
    { "cc430f5137",2,8 },
    { "cc430f5143",2,8 },
    { "cc430f5145",2,8 },
    { "cc430f5147",2,8 },
    { "cc430f6125",2,8 },
    { "cc430f6126",2,8 },
    { "cc430f6127",2,8 },
    { "cc430f6135",2,8 },
    { "cc430f6137",2,8 },
    { "cc430f6143",2,8 },
    { "cc430f6145",2,8 },
    { "cc430f6147",2,8 },
    { "msp430afe221",0,2 },
    { "msp430afe222",0,2 },
    { "msp430afe223",0,2 },
    { "msp430afe231",0,2 },
    { "msp430afe232",0,2 },
    { "msp430afe233",0,2 },
    { "msp430afe251",0,2 },
    { "msp430afe252",0,2 },
    { "msp430afe253",0,2 },
    { "msp430bt5190",2,8 },
    { "msp430c091",0,0 },
    { "msp430c092",0,0 },
    { "msp430c111",0,0 },
    { "msp430c1111",0,0 },
    { "msp430c112",0,0 },
    { "msp430c1121",0,0 },
    { "msp430c1331",0,0 },
    { "msp430c1351",0,0 },
    { "msp430c311s",0,0 },
    { "msp430c312",0,0 },
    { "msp430c313",0,0 },
    { "msp430c314",0,0 },
    { "msp430c315",0,0 },
    { "msp430c323",0,0 },
    { "msp430c325",0,0 },
    { "msp430c336",0,1 },
    { "msp430c337",0,1 },
    { "msp430c412",0,0 },
    { "msp430c413",0,0 },
    { "msp430cg4616",1,1 },
    { "msp430cg4617",1,1 },
    { "msp430cg4618",1,1 },
    { "msp430cg4619",1,1 },
    { "msp430e112",0,0 },
    { "msp430e313",0,0 },
    { "msp430e315",0,0 },
    { "msp430e325",0,0 },
    { "msp430e337",0,1 },
    { "msp430f110",0,0 },
    { "msp430f1101",0,0 },
    { "msp430f1101a",0,0 },
    { "msp430f1111",0,0 },
    { "msp430f1111a",0,0 },
    { "msp430f112",0,0 },
    { "msp430f1121",0,0 },
    { "msp430f1121a",0,0 },
    { "msp430f1122",0,0 },
    { "msp430f1132",0,0 },
    { "msp430f122",0,0 },
    { "msp430f1222",0,0 },
    { "msp430f123",0,0 },
    { "msp430f1232",0,0 },
    { "msp430f133",0,0 },
    { "msp430f135",0,0 },
    { "msp430f147",0,1 },
    { "msp430f1471",0,1 },
    { "msp430f148",0,1 },
    { "msp430f1481",0,1 },
    { "msp430f149",0,1 },
    { "msp430f1491",0,1 },
    { "msp430f155",0,0 },
    { "msp430f156",0,0 },
    { "msp430f157",0,0 },
    { "msp430f1610",0,1 },
    { "msp430f1611",0,1 },
    { "msp430f1612",0,1 },
    { "msp430f167",0,1 },
    { "msp430f168",0,1 },
    { "msp430f169",0,1 },
    { "msp430f2001",0,0 },
    { "msp430f2002",0,0 },
    { "msp430f2003",0,0 },
    { "msp430f2011",0,0 },
    { "msp430f2012",0,0 },
    { "msp430f2013",0,0 },
    { "msp430f2101",0,0 },
    { "msp430f2111",0,0 },
    { "msp430f2112",0,0 },
    { "msp430f2121",0,0 },
    { "msp430f2122",0,0 },
    { "msp430f2131",0,0 },
    { "msp430f2132",0,0 },
    { "msp430f2232",0,0 },
    { "msp430f2234",0,0 },
    { "msp430f2252",0,0 },
    { "msp430f2254",0,0 },
    { "msp430f2272",0,0 },
    { "msp430f2274",0,0 },
    { "msp430f233",0,2 },
    { "msp430f2330",0,2 },
    { "msp430f235",0,2 },
    { "msp430f2350",0,2 },
    { "msp430f2370",0,2 },
    { "msp430f2410",0,2 },
    { "msp430f2416",1,2 },
    { "msp430f2417",1,2 },
    { "msp430f2418",1,2 },
    { "msp430f2419",1,2 },
    { "msp430f247",0,2 },
    { "msp430f2471",0,2 },
    { "msp430f248",0,2 },
    { "msp430f2481",0,2 },
    { "msp430f249",0,2 },
    { "msp430f2491",0,2 },
    { "msp430f2616",1,2 },
    { "msp430f2617",1,2 },
    { "msp430f2618",1,2 },
    { "msp430f2619",1,2 },
    { "msp430f412",0,0 },
    { "msp430f413",0,0 },
    { "msp430f4132",0,0 },
    { "msp430f415",0,0 },
    { "msp430f4152",0,0 },
    { "msp430f417",0,0 },
    { "msp430f423",0,1 },
    { "msp430f423a",0,1 },
    { "msp430f425",0,1 },
    { "msp430f4250",0,0 },
    { "msp430f425a",0,1 },
    { "msp430f4260",0,0 },
    { "msp430f427",0,1 },
    { "msp430f4270",0,0 },
    { "msp430f427a",0,1 },
    { "msp430f435",0,0 },
    { "msp430f4351",0,0 },
    { "msp430f436",0,0 },
    { "msp430f4361",0,0 },
    { "msp430f437",0,0 },
    { "msp430f4371",0,0 },
    { "msp430f438",0,0 },
    { "msp430f439",0,0 },
    { "msp430f447",0,1 },
    { "msp430f448",0,1 },
    { "msp430f4481",0,1 },
    { "msp430f449",0,1 },
    { "msp430f4491",0,1 },
    { "msp430f4616",1,1 },
    { "msp430f46161",1,1 },
    { "msp430f4617",1,1 },
    { "msp430f46171",1,1 },
    { "msp430f4618",1,1 },
    { "msp430f46181",1,1 },
    { "msp430f4619",1,1 },
    { "msp430f46191",1,1 },
    { "msp430f47126",1,4 },
    { "msp430f47127",1,4 },
    { "msp430f47163",1,4 },
    { "msp430f47166",1,4 },
    { "msp430f47167",1,4 },
    { "msp430f47173",1,4 },
    { "msp430f47176",1,4 },
    { "msp430f47177",1,4 },
    { "msp430f47183",1,4 },
    { "msp430f47186",1,4 },
    { "msp430f47187",1,4 },
    { "msp430f47193",1,4 },
    { "msp430f47196",1,4 },
    { "msp430f47197",1,4 },
    { "msp430f477",0,0 },
    { "msp430f478",0,0 },
    { "msp430f4783",0,4 },
    { "msp430f4784",0,4 },
    { "msp430f479",0,0 },
    { "msp430f4793",0,4 },
    { "msp430f4794",0,4 },
    { "msp430f5131",2,8 },
    { "msp430f5132",2,8 },
    { "msp430f5151",2,8 },
    { "msp430f5152",2,8 },
    { "msp430f5171",2,8 },
    { "msp430f5172",2,8 },
    { "msp430f5212",2,8 },
    { "msp430f5213",2,8 },
    { "msp430f5214",2,8 },
    { "msp430f5217",2,8 },
    { "msp430f5218",2,8 },
    { "msp430f5219",2,8 },
    { "msp430f5222",2,8 },
    { "msp430f5223",2,8 },
    { "msp430f5224",2,8 },
    { "msp430f5227",2,8 },
    { "msp430f5228",2,8 },
    { "msp430f5229",2,8 },
    { "msp430f5232",2,8 },
    { "msp430f5234",2,8 },
    { "msp430f5237",2,8 },
    { "msp430f5239",2,8 },
    { "msp430f5242",2,8 },
    { "msp430f5244",2,8 },
    { "msp430f5247",2,8 },
    { "msp430f5249",2,8 },
    { "msp430f5252",2,8 },
    { "msp430f5253",2,8 },
    { "msp430f5254",2,8 },
    { "msp430f5255",2,8 },
    { "msp430f5256",2,8 },
    { "msp430f5257",2,8 },
    { "msp430f5258",2,8 },
    { "msp430f5259",2,8 },
    { "msp430f5304",2,8 },
    { "msp430f5308",2,8 },
    { "msp430f5309",2,8 },
    { "msp430f5310",2,8 },
    { "msp430f5324",2,8 },
    { "msp430f5325",2,8 },
    { "msp430f5326",2,8 },
    { "msp430f5327",2,8 },
    { "msp430f5328",2,8 },
    { "msp430f5329",2,8 },
    { "msp430f5333",2,8 },
    { "msp430f5335",2,8 },
    { "msp430f5336",2,8 },
    { "msp430f5338",2,8 },
    { "msp430f5340",2,8 },
    { "msp430f5341",2,8 },
    { "msp430f5342",2,8 },
    { "msp430f5358",2,8 },
    { "msp430f5359",2,8 },
    { "msp430f5418",2,8 },
    { "msp430f5418a",2,8 },
    { "msp430f5419",2,8 },
    { "msp430f5419a",2,8 },
    { "msp430f5435",2,8 },
    { "msp430f5435a",2,8 },
    { "msp430f5436",2,8 },
    { "msp430f5436a",2,8 },
    { "msp430f5437",2,8 },
    { "msp430f5437a",2,8 },
    { "msp430f5438",2,8 },
    { "msp430f5438a",2,8 },
    { "msp430f5500",2,8 },
    { "msp430f5501",2,8 },
    { "msp430f5502",2,8 },
    { "msp430f5503",2,8 },
    { "msp430f5504",2,8 },
    { "msp430f5505",2,8 },
    { "msp430f5506",2,8 },
    { "msp430f5507",2,8 },
    { "msp430f5508",2,8 },
    { "msp430f5509",2,8 },
    { "msp430f5510",2,8 },
    { "msp430f5513",2,8 },
    { "msp430f5514",2,8 },
    { "msp430f5515",2,8 },
    { "msp430f5517",2,8 },
    { "msp430f5519",2,8 },
    { "msp430f5521",2,8 },
    { "msp430f5522",2,8 },
    { "msp430f5524",2,8 },
    { "msp430f5525",2,8 },
    { "msp430f5526",2,8 },
    { "msp430f5527",2,8 },
    { "msp430f5528",2,8 },
    { "msp430f5529",2,8 },
    { "msp430f5630",2,8 },
    { "msp430f5631",2,8 },
    { "msp430f5632",2,8 },
    { "msp430f5633",2,8 },
    { "msp430f5634",2,8 },
    { "msp430f5635",2,8 },
    { "msp430f5636",2,8 },
    { "msp430f5637",2,8 },
    { "msp430f5638",2,8 },
    { "msp430f5658",2,8 },
    { "msp430f5659",2,8 },
    { "msp430f5xx_6xxgeneric",2,8 },
    { "msp430f6433",2,8 },
    { "msp430f6435",2,8 },
    { "msp430f6436",2,8 },
    { "msp430f6438",2,8 },
    { "msp430f6458",2,8 },
    { "msp430f6459",2,8 },
    { "msp430f6630",2,8 },
    { "msp430f6631",2,8 },
    { "msp430f6632",2,8 },
    { "msp430f6633",2,8 },
    { "msp430f6634",2,8 },
    { "msp430f6635",2,8 },
    { "msp430f6636",2,8 },
    { "msp430f6637",2,8 },
    { "msp430f6638",2,8 },
    { "msp430f6658",2,8 },
    { "msp430f6659",2,8 },
    { "msp430f6720",2,8 },
    { "msp430f6720a",2,8 },
    { "msp430f6721",2,8 },
    { "msp430f6721a",2,8 },
    { "msp430f6723",2,8 },
    { "msp430f6723a",2,8 },
    { "msp430f6724",2,8 },
    { "msp430f6724a",2,8 },
    { "msp430f6725",2,8 },
    { "msp430f6725a",2,8 },
    { "msp430f6726",2,8 },
    { "msp430f6726a",2,8 },
    { "msp430f6730",2,8 },
    { "msp430f6730a",2,8 },
    { "msp430f6731",2,8 },
    { "msp430f6731a",2,8 },
    { "msp430f6733",2,8 },
    { "msp430f6733a",2,8 },
    { "msp430f6734",2,8 },
    { "msp430f6734a",2,8 },
    { "msp430f6735",2,8 },
    { "msp430f6735a",2,8 },
    { "msp430f6736",2,8 },
    { "msp430f6736a",2,8 },
    { "msp430f6745",2,8 },
    { "msp430f67451",2,8 },
    { "msp430f67451a",2,8 },
    { "msp430f6745a",2,8 },
    { "msp430f6746",2,8 },
    { "msp430f67461",2,8 },
    { "msp430f67461a",2,8 },
    { "msp430f6746a",2,8 },
    { "msp430f6747",2,8 },
    { "msp430f67471",2,8 },
    { "msp430f67471a",2,8 },
    { "msp430f6747a",2,8 },
    { "msp430f6748",2,8 },
    { "msp430f67481",2,8 },
    { "msp430f67481a",2,8 },
    { "msp430f6748a",2,8 },
    { "msp430f6749",2,8 },
    { "msp430f67491",2,8 },
    { "msp430f67491a",2,8 },
    { "msp430f6749a",2,8 },
    { "msp430f67621",2,8 },
    { "msp430f67621a",2,8 },
    { "msp430f67641",2,8 },
    { "msp430f67641a",2,8 },
    { "msp430f6765",2,8 },
    { "msp430f67651",2,8 },
    { "msp430f67651a",2,8 },
    { "msp430f6765a",2,8 },
    { "msp430f6766",2,8 },
    { "msp430f67661",2,8 },
    { "msp430f67661a",2,8 },
    { "msp430f6766a",2,8 },
    { "msp430f6767",2,8 },
    { "msp430f67671",2,8 },
    { "msp430f67671a",2,8 },
    { "msp430f6767a",2,8 },
    { "msp430f6768",2,8 },
    { "msp430f67681",2,8 },
    { "msp430f67681a",2,8 },
    { "msp430f6768a",2,8 },
    { "msp430f6769",2,8 },
    { "msp430f67691",2,8 },
    { "msp430f67691a",2,8 },
    { "msp430f6769a",2,8 },
    { "msp430f6775",2,8 },
    { "msp430f67751",2,8 },
    { "msp430f67751a",2,8 },
    { "msp430f6775a",2,8 },
    { "msp430f6776",2,8 },
    { "msp430f67761",2,8 },
    { "msp430f67761a",2,8 },
    { "msp430f6776a",2,8 },
    { "msp430f6777",2,8 },
    { "msp430f67771",2,8 },
    { "msp430f67771a",2,8 },
    { "msp430f6777a",2,8 },
    { "msp430f6778",2,8 },
    { "msp430f67781",2,8 },
    { "msp430f67781a",2,8 },
    { "msp430f6778a",2,8 },
    { "msp430f6779",2,8 },
    { "msp430f67791",2,8 },
    { "msp430f67791a",2,8 },
    { "msp430f6779a",2,8 },
    { "msp430fe423",0,0 },
    { "msp430fe4232",0,0 },
    { "msp430fe423a",0,0 },
    { "msp430fe4242",0,0 },
    { "msp430fe425",0,0 },
    { "msp430fe4252",0,0 },
    { "msp430fe425a",0,0 },
    { "msp430fe427",0,0 },
    { "msp430fe4272",0,0 },
    { "msp430fe427a",0,0 },
    { "msp430fg4250",0,0 },
    { "msp430fg4260",0,0 },
    { "msp430fg4270",0,0 },
    { "msp430fg437",0,0 },
    { "msp430fg438",0,0 },
    { "msp430fg439",0,0 },
    { "msp430fg4616",1,1 },
    { "msp430fg4617",1,1 },
    { "msp430fg4618",1,1 },
    { "msp430fg4619",1,1 },
    { "msp430fg477",0,0 },
    { "msp430fg478",0,0 },
    { "msp430fg479",0,0 },
    { "msp430fg6425",2,8 },
    { "msp430fg6426",2,8 },
    { "msp430fg6625",2,8 },
    { "msp430fg6626",2,8 },
    { "msp430fr2032",2,0 },
    { "msp430fr2033",2,0 },
    { "msp430fr2110",2,0 },
    { "msp430fr2111",2,0 },
    { "msp430fr2310",2,0 },
    { "msp430fr2311",2,0 },
    { "msp430fr2433",2,8 },
    { "msp430fr2532",2,8 },
    { "msp430fr2533",2,8 },
    { "msp430fr2632",2,8 },
    { "msp430fr2633",2,8 },
    { "msp430fr2xx_4xxgeneric",2,8 },
    { "msp430fr4131",2,0 },
    { "msp430fr4132",2,0 },
    { "msp430fr4133",2,0 },
    { "msp430fr5720",2,8 },
    { "msp430fr5721",2,8 },
    { "msp430fr5722",2,8 },
    { "msp430fr5723",2,8 },
    { "msp430fr5724",2,8 },
    { "msp430fr5725",2,8 },
    { "msp430fr5726",2,8 },
    { "msp430fr5727",2,8 },
    { "msp430fr5728",2,8 },
    { "msp430fr5729",2,8 },
    { "msp430fr5730",2,8 },
    { "msp430fr5731",2,8 },
    { "msp430fr5732",2,8 },
    { "msp430fr5733",2,8 },
    { "msp430fr5734",2,8 },
    { "msp430fr5735",2,8 },
    { "msp430fr5736",2,8 },
    { "msp430fr5737",2,8 },
    { "msp430fr5738",2,8 },
    { "msp430fr5739",2,8 },
    { "msp430fr57xxgeneric",2,8 },
    { "msp430fr5847",2,8 },
    { "msp430fr58471",2,8 },
    { "msp430fr5848",2,8 },
    { "msp430fr5849",2,8 },
    { "msp430fr5857",2,8 },
    { "msp430fr5858",2,8 },
    { "msp430fr5859",2,8 },
    { "msp430fr5867",2,8 },
    { "msp430fr58671",2,8 },
    { "msp430fr5868",2,8 },
    { "msp430fr5869",2,8 },
    { "msp430fr5870",2,8 },
    { "msp430fr5872",2,8 },
    { "msp430fr58721",2,8 },
    { "msp430fr5887",2,8 },
    { "msp430fr5888",2,8 },
    { "msp430fr5889",2,8 },
    { "msp430fr58891",2,8 },
    { "msp430fr5922",2,8 },
    { "msp430fr59221",2,8 },
    { "msp430fr5947",2,8 },
    { "msp430fr59471",2,8 },
    { "msp430fr5948",2,8 },
    { "msp430fr5949",2,8 },
    { "msp430fr5957",2,8 },
    { "msp430fr5958",2,8 },
    { "msp430fr5959",2,8 },
    { "msp430fr5962",2,8 },
    { "msp430fr5964",2,8 },
    { "msp430fr5967",2,8 },
    { "msp430fr5968",2,8 },
    { "msp430fr5969",2,8 },
    { "msp430fr59691",2,8 },
    { "msp430fr5970",2,8 },
    { "msp430fr5972",2,8 },
    { "msp430fr59721",2,8 },
    { "msp430fr5986",2,8 },
    { "msp430fr5987",2,8 },
    { "msp430fr5988",2,8 },
    { "msp430fr5989",2,8 },
    { "msp430fr59891",2,8 },
    { "msp430fr5992",2,8 },
    { "msp430fr5994",2,8 },
    { "msp430fr59941",2,8 },
    { "msp430fr5xx_6xxgeneric",2,8 },
    { "msp430fr6820",2,8 },
    { "msp430fr6822",2,8 },
    { "msp430fr68221",2,8 },
    { "msp430fr6870",2,8 },
    { "msp430fr6872",2,8 },
    { "msp430fr68721",2,8 },
    { "msp430fr6877",2,8 },
    { "msp430fr6879",2,8 },
    { "msp430fr68791",2,8 },
    { "msp430fr6887",2,8 },
    { "msp430fr6888",2,8 },
    { "msp430fr6889",2,8 },
    { "msp430fr68891",2,8 },
    { "msp430fr6920",2,8 },
    { "msp430fr6922",2,8 },
    { "msp430fr69221",2,8 },
    { "msp430fr6927",2,8 },
    { "msp430fr69271",2,8 },
    { "msp430fr6928",2,8 },
    { "msp430fr6970",2,8 },
    { "msp430fr6972",2,8 },
    { "msp430fr69721",2,8 },
    { "msp430fr6977",2,8 },
    { "msp430fr6979",2,8 },
    { "msp430fr69791",2,8 },
    { "msp430fr6987",2,8 },
    { "msp430fr6988",2,8 },
    { "msp430fr6989",2,8 },
    { "msp430fr69891",2,8 },
    { "msp430fw423",0,0 },
    { "msp430fw425",0,0 },
    { "msp430fw427",0,0 },
    { "msp430fw428",0,0 },
    { "msp430fw429",0,0 },
    { "msp430g2001",0,0 },
    { "msp430g2101",0,0 },
    { "msp430g2102",0,0 },
    { "msp430g2111",0,0 },
    { "msp430g2112",0,0 },
    { "msp430g2113",0,0 },
    { "msp430g2121",0,0 },
    { "msp430g2131",0,0 },
    { "msp430g2132",0,0 },
    { "msp430g2152",0,0 },
    { "msp430g2153",0,0 },
    { "msp430g2201",0,0 },
    { "msp430g2202",0,0 },
    { "msp430g2203",0,0 },
    { "msp430g2210",0,0 },
    { "msp430g2211",0,0 },
    { "msp430g2212",0,0 },
    { "msp430g2213",0,0 },
    { "msp430g2221",0,0 },
    { "msp430g2230",0,0 },
    { "msp430g2231",0,0 },
    { "msp430g2232",0,0 },
    { "msp430g2233",0,0 },
    { "msp430g2252",0,0 },
    { "msp430g2253",0,0 },
    { "msp430g2302",0,0 },
    { "msp430g2303",0,0 },
    { "msp430g2312",0,0 },
    { "msp430g2313",0,0 },
    { "msp430g2332",0,0 },
    { "msp430g2333",0,0 },
    { "msp430g2352",0,0 },
    { "msp430g2353",0,0 },
    { "msp430g2402",0,0 },
    { "msp430g2403",0,0 },
    { "msp430g2412",0,0 },
    { "msp430g2413",0,0 },
    { "msp430g2432",0,0 },
    { "msp430g2433",0,0 },
    { "msp430g2444",0,0 },
    { "msp430g2452",0,0 },
    { "msp430g2453",0,0 },
    { "msp430g2513",0,0 },
    { "msp430g2533",0,0 },
    { "msp430g2544",0,0 },
    { "msp430g2553",0,0 },
    { "msp430g2744",0,0 },
    { "msp430g2755",0,0 },
    { "msp430g2855",0,0 },
    { "msp430g2955",0,0 },
    { "msp430i2020",0,2 },
    { "msp430i2021",0,2 },
    { "msp430i2030",0,2 },
    { "msp430i2031",0,2 },
    { "msp430i2040",0,2 },
    { "msp430i2041",0,2 },
    { "msp430i2xxgeneric",0,2 },
    { "msp430l092",0,0 },
    { "msp430p112",0,0 },
    { "msp430p313",0,0 },
    { "msp430p315",0,0 },
    { "msp430p315s",0,0 },
    { "msp430p325",0,0 },
    { "msp430p337",0,1 },
    { "msp430sl5438a",2,8 },
    { "msp430tch5e",0,0 },
    { "msp430xgeneric",2,8 },
    { "rf430f5144",2,8 },
    { "rf430f5155",2,8 },
    { "rf430f5175",2,8 },
    { "rf430frl152h",0,0 },
    { "rf430frl152h_rom",0,0 },
    { "rf430frl153h",0,0 },
    { "rf430frl153h_rom",0,0 },
    { "rf430frl154h",0,0 },
    { "rf430frl154h_rom",0,0 }
  };
