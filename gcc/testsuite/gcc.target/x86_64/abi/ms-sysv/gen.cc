/* Test program generator for 64-bit Microsoft ABI.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Daniel Santos <daniel.santos@pobox.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <cstdio>
#include <cassert>
#include <vector>
#include <string>
#include <cstring>
#include <iostream>
#include <algorithm>
#include <ios>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <memory>
#include <regex>
#include <stdexcept>

#include <unistd.h>
#include <getopt.h>

using namespace std;

/* A basic Effective C++ Item 6. */
class uncopyable
{
private:
  uncopyable (const uncopyable &) = delete;
  const uncopyable& operator= (const uncopyable &) = delete;

protected:
  uncopyable() {}
  ~uncopyable() {}
};

/* A simple class for adding text delimiters.  */
class list_delimiter : protected uncopyable
{
  int m_pos;
  string m_delim;
  static string s_empty;

  list_delimiter ();

public:
  list_delimiter (const char *delim, int init_pos = 0)
      : m_pos (init_pos), m_delim(delim) {}
  const string &get ()	{return m_pos++ ? m_delim : s_empty;}
  void reset () 	{m_pos = 0;}
  int get_pos ()	{return m_pos;}
};

string list_delimiter::s_empty = "";

/* Bitmasks for representing non-volatile retisters of an ms_abi call that
   are not already clobbered by a sysv_abi call.  */
enum optional_regs
{
  OPTIONAL_REG_RBX = 0x01,
  OPTIONAL_REG_RBP = 0x02,
  OPTIONAL_REG_R12 = 0x04,
  OPTIONAL_REG_R13 = 0x08,
  OPTIONAL_REG_R14 = 0x10,
  OPTIONAL_REG_R15 = 0x20,

  OPTIONAL_REG_ALL = 0x3f,
  OPTIONAL_REG_HFP_ALL = OPTIONAL_REG_ALL & (~OPTIONAL_REG_RBP)
};

static const char * const optional_regs_str[] = {
  "rbx",
  "rbp",
  "r12",
  "r13",
  "r14",
  "r15",
};

/* A simple type & name representation of a function parameter.  */
class arg
{
  string name;
  string type;
  bool type_is_integral:1;

public:
  arg(const char *name, const char *type, bool type_is_integral);

  bool is_type_integral () const	{return type_is_integral;}
  const string &get_name () const	{return name;}
  const string &get_type () const	{return type;}
};

arg::arg(const char *name, const char *type, bool type_is_integral)
    : name (name), type (type), type_is_integral (type_is_integral)
{
}

/* A stupid operator<< implementation for arg objects.  */
template<class T> T &operator<< (T &out, const arg &a)
{
  return out << a.get_type () << " " << a.get_name ();
}

/* Bitmask representation of all possible varients of a test function. The
   value FN_VAR_MSABI is only used internally to distinguish between an
   ms_abi and sysv_abi function.  */
enum fn_variants {
  FN_VAR_MSABI		= 0x01,
  FN_VAR_HFP		= 0x02,
  FN_VAR_REALIGN	= 0x04,
  FN_VAR_ALLOCA		= 0x08,
  FN_VAR_VARARGS	= 0x10,
  FN_VAR_SIBCALL	= 0x20,
  FN_VAR_SHRINK_WRAP	= 0x40,

  FN_VAR_HFP_OR_REALIGN	= FN_VAR_HFP | FN_VAR_REALIGN,
  FN_VAR_MASK		= 0x7f,
  FN_VAR_COUNT		= 7
};

/* Representation of a Microsoft or System V ABI function with varying
   parameters, quirks and optimization goals.

   Function name nomenclature:
     (msabi|sysv)_[xx_][r|f][a][v][s][w]<n>
      |            |    |    |  |  |  |  |
      |            |    |    |  |  |  |  Number of extra (long) parameters
      |            |    |    |  |  |  shrink wrap
      |            |    |    |  |  sibling call
      |            |    |    |  varargs
      |            |    |    alloca
      |            |    Forced realignment or hard frame pointer
      |            Explicit clobbers (hexidecimal mask, ms_abi only)
      Calling Convention  */
class fn : protected uncopyable
{
private:
  const vector<arg> &m_args;
  string m_name;
  string m_attr_decl_str;
  string m_attr_def_str;
  int m_clobbers:FN_VAR_COUNT;
  int m_var;

public:
  fn (const vector<arg> &args, int clobbers, int var);

  void print_params (ostream &out) const;
  void print_decl (ostream &out, bool for_def = false) const;
  void print_noinfo_def (ostream &out) const;
  void print_def (ostream &out) const;
  const string &get_name () const	{return m_name;}
  const vector<arg> &get_args () const	{return m_args;}

  bool get_hfp_or_realign () const	{return m_var & FN_VAR_HFP_OR_REALIGN;}
  bool get_msabi () const		{return m_var & FN_VAR_MSABI;}
  bool get_hfp () const			{return m_var & FN_VAR_HFP;}
  bool get_realign () const		{return m_var & FN_VAR_REALIGN;}
  bool get_alloca () const		{return m_var & FN_VAR_ALLOCA;}
  bool get_varargs () const		{return m_var & FN_VAR_VARARGS;}
  bool get_sibcall () const		{return m_var & FN_VAR_SIBCALL;}
  bool get_shrink_wrap () const		{return m_var & FN_VAR_SHRINK_WRAP;}
};

fn::fn (const vector<arg> &args, int clobbers, int var)
    : m_args (args)
    , m_name ()
    , m_attr_decl_str ()
    , m_attr_def_str ("noinline")
    , m_clobbers (clobbers)
    , m_var (var)
{
  assert (!(var & ~FN_VAR_MASK));

  if (get_hfp () && get_realign ())
    throw invalid_argument ("`hfp' with `realign' does nothing.");

  if (get_varargs () && args.empty ())
    throw invalid_argument ("Need at least one normal argument to use varargs");

  assert (!(get_hfp () || get_realign ()) || !(clobbers & OPTIONAL_REG_RBP));

  stringstream name;
  name << (get_msabi () ? "msabi_" : "sysv_");
  if (get_msabi ())
    name << setfill('0') << setw(2) << hex << m_clobbers << "_";
  name << (get_realign () ? "r" : (get_hfp () ? "f" : ""))
       << (get_alloca () ? "a" : "")
       << (get_varargs () ? "v" : "")
       << (get_sibcall () ? "s" : "")
       << (get_shrink_wrap () ? "w" : "")
       << setw(0) << dec << (unsigned)args.size();
  m_name = name.str();

  list_delimiter decl_comma (", ", !m_attr_decl_str.empty ());
  list_delimiter def_comma (", ", !m_attr_def_str.empty ());
  if (get_msabi ())
    {
	m_attr_decl_str += decl_comma.get ();
	m_attr_decl_str += "ms_abi";
	m_attr_def_str += def_comma.get ();
	m_attr_def_str += "ms_abi";
    }

  if (get_realign ())
    {
      m_attr_def_str += def_comma.get();
      m_attr_def_str += "__force_align_arg_pointer__";
    }
  else if (get_hfp ())
    {
      m_attr_def_str += def_comma.get();
      m_attr_def_str += "optimize (\"no-omit-frame-pointer\")";
    }
}

/* Print the parameters for a function declaration.  */
void fn::print_params (ostream &out) const
{
  list_delimiter comma (", ");

  vector<arg>::const_iterator i;
  if (get_alloca () && !get_msabi ())
    out << comma.get () << "void *alloca_mem";
  for (i = m_args.begin(); i != m_args.end(); ++i)
    out << comma.get () << *i;

  if (get_varargs ())
    out << comma.get () << (get_msabi () ? "..." : "va_list argptr");
}

/* Print the declaration for a function.  */
void fn::print_decl (ostream &out, bool for_def) const
{
  const string &attr_str = (for_def ? m_attr_def_str : m_attr_decl_str);
  if (!for_def)
    out << "extern ";

  if (!attr_str.empty ())
    out << "__attribute__ ((" << attr_str << ")) ";

  out << "long " << m_name << " (";
  print_params (out);
  out << ")";
  if (!for_def)
    out << ";" << endl;
}

/* Output a volatile "_noinfo" function pointer definition.  */
void fn::print_noinfo_def (ostream &out) const
{
  out << "static ";
  if (!m_attr_decl_str.empty ())
    out << "__attribute__ ((" << m_attr_decl_str << ")) ";
  out << "long (*const volatile " << m_name << "_noinfo) (";
  print_params (out);
  out << ") = " << m_name << ";" << endl;
}

/* Print the definition of a function.  */
void fn::print_def (ostream &out) const
{
  vector<arg>::const_iterator i;

  print_decl (out, true);
  out << endl << "{" << endl;

  if (get_msabi () && get_alloca ())
    {
      const char *size_str = m_args.empty () ? "42" : "a";
      out << "  void *alloca_mem = __builtin_alloca (8 + " << size_str << ");" << endl
	  << "  *(long*)alloca_mem = FLAG_ALLOCA;" << endl;
    }
  if (get_msabi () && get_varargs ())
    out << "  va_list argptr;" << endl;
  if (get_shrink_wrap ())
    out << "  if (shrink_wrap_global == FLAG_SHRINK_WRAP_FAST_PATH)" << endl
	<< "    return FLAG_SHRINK_WRAP_FAST_PATH;" << endl;

  list_delimiter comma (", ");
  if (m_clobbers)
    {
      out << "  __asm__ __volatile__ (\"\" :::";
      unsigned c;
      unsigned mask = m_clobbers;
      comma.reset ();
      for (c = 0, mask = m_clobbers; mask; ++c, mask >>= 1)
	if (mask & 1)
	  out << comma.get () << "\"" << optional_regs_str[c] << "\"";
      out << ");" << endl;
    }

  if (get_msabi () && get_varargs ())
    {
      assert (!m_args.empty ());
      out << "  va_start(argptr, " << m_args.back ().get_name () << ");" << endl;
    }

  out << "  return ";
  if (get_msabi ())
    {
      if (get_sibcall ())
	out << "do_sibcall_noinfo (";

      comma.reset ();
      out << "sysv_"
	  << (get_alloca () ? "a" : "")
	  << (get_varargs () ? "v" : "")
	  << m_args.size ()
	  << "_noinfo (";

      if (get_alloca ())
	out << comma.get () << "alloca_mem";
      for (i = m_args.begin(); i != m_args.end(); ++i)
	out << comma.get () << i->get_name ();
      if (get_varargs ())
	out << comma.get () << "argptr";
      out << ")";
      if (get_shrink_wrap ())
	out << " + FLAG_SHRINK_WRAP_SLOW_PATH";
      if (get_sibcall ())
	out << ")";
    }
  else
    {
      list_delimiter plus (" + ");
      for (i = m_args.begin(); i != m_args.end(); ++i)
	  if (i->is_type_integral ())
	    out << plus.get () << i->get_name ();
      if (get_alloca ())
	out << plus.get () << "*(long*)alloca_mem";
      if (!plus.get_pos ())
	out << "0";
    }
  out << ";" << endl;
  if (get_msabi () && get_varargs ())
    out << "  va_end(argptr);" << endl;
  out << "}" << endl << endl;
}

/* Global variables.  */
string argv0;
string out_file_name;
unsigned int extra_params_min = 0;
unsigned int extra_params_max = 5;
unsigned fn_variant_mask = FN_VAR_MASK;
bool omit_rbp_clobbers = false;
vector<class fn*> sysv_funcs;
vector<class fn*> msabi_funcs;


/* Emit extern for do_test_aligned and do_test_unaligned (defined in do_test.S)
   followed by all of the various do_test* function function pointers that
   are just aliases of them.  */
static void make_do_tests_decl (const vector<class arg> &args, ostream &out)
{
  vector<class arg>::const_iterator ai;
  unsigned i, varargs, unaligned;

  out << "extern __attribute__ ((ms_abi)) long do_test_aligned ();" << endl
      << "extern __attribute__ ((ms_abi)) long do_test_unaligned ();" << endl;

  list_delimiter comma (", ");
  for (i = extra_params_min; i <= args.size (); ++i)
    for (unaligned = 0; unaligned <= 1; ++unaligned)
      for (varargs = 0; varargs <= 1; ++varargs)
	{
	  if (!i && varargs)  /* skip varargs version when no other args */
	    continue;

	  comma.reset ();
	  out << "static __attribute__ ((ms_abi)) long (*do_test_"
	      << (unaligned ? "u" : "")
	      << (varargs ? "v" : "") << i << ") (";

	  unsigned j;
	  for (j = 0, ai = args.begin (); j < i; ++j, ++ai)
	    out << comma.get () << ai->get_type () << " "
		<< ai->get_name ();
	  if (varargs)
	    out << comma.get () << "...";
	  out << ") = (void*)do_test_" << (unaligned ? "un" : "")
	      << "aligned;" << endl;
	}
}

/* Generate do_tests function.  We actually break it up into multiple
   do_test_xxxx functions to keep compile times down (with just one large
   function, it is a very slow build).  */
void make_do_test (const vector<class arg> &args,
		   const vector<class fn*> &msabi_funcs,
		   ostream &out)
{
  const unsigned TESTS_PER_FN_MAX = 64;
  unsigned i;
  vector<string> do_tests_fn_names;
  unsigned fn_count = 0;
  unsigned test_count = TESTS_PER_FN_MAX;
  string params_str;
  string param_names_str;
  string param_types_str;

  /* Init some commonly used strings.  */
  {
    stringstream s1, s2, s3;
    list_delimiter comma(", ");
    for (auto arg : args)
      {
	const string &c = comma.get ();
	s1 << c << arg;
	s2 << c << arg.get_name ();
	s3 << c << arg.get_type ();
      }
    params_str = s1.str ();
    param_names_str = s2.str ();
    param_types_str = s3.str ();
  }

  vector<class fn*>::const_iterator fi;
  for (fi = msabi_funcs.begin(); fi != msabi_funcs.end(); ++fi)
    {
      const fn &f = **fi;
      unsigned unaligned, shrink_wrap;

      for (unaligned = 0; unaligned <= !!f.get_realign (); ++unaligned)
	for (shrink_wrap = 0; shrink_wrap <= !!f.get_shrink_wrap ();
	     ++shrink_wrap)
	  {
	    const vector<class arg> &fargs = f.get_args ();

	    /* To prevent unwieldy build times, we split up tests to 64-ish per
	       function.  */
	    if (++test_count > TESTS_PER_FN_MAX)
	      {
		test_count = 1;
		if (fn_count > 0) {
		  out << "}" << endl << endl;
		}

		stringstream fn_name;
		fn_name << "do_tests_" << setfill('0') << setw(4) << hex
		     << fn_count++;
		do_tests_fn_names.push_back (fn_name.str ());

		out << "static __attribute__((noinline)) void "
		    << fn_name.str () << " (" << params_str << ")" << endl
		    << "{" << endl
		    << "  long ret;" << endl;
	      }

	    /* Call init_test.  */
	    out << endl
		<< "  init_test (" << f.get_name () << ", \""
		<< f.get_name () << "\", ";

	    if (f.get_realign ())
	      out << (unaligned ? "ALIGNMENT_MISALIGNED"
				: "ALIGNMENT_ALIGNED");
	    else
	      out << "ALIGNMENT_NOT_TESTED";

	    out << ", ";
	    if (f.get_shrink_wrap ())
	      out << (shrink_wrap ? "SHRINK_WRAP_SLOW_PATH"
				  : "SHRINK_WRAP_FAST_PATH");
	    else
	      out << "SHRINK_WRAP_NONE";
	    out << ", ";

	    /* Calculated the expected return value.  */
	    if (f.get_shrink_wrap () && shrink_wrap == 0)
	      out << "FLAG_SHRINK_WRAP_FAST_PATH";
	    else
	      {
		list_delimiter plus (" + ");
		for (auto const &arg : fargs)
		  out << plus.get () << arg.get_name ();
		if (f.get_sibcall ())
		  out << plus.get () << "FLAG_SIBCALL";
		if (f.get_alloca ())
		  out << plus.get () << "FLAG_ALLOCA";
		if (f.get_shrink_wrap () && shrink_wrap == 1)
		  out << plus.get () << "FLAG_SHRINK_WRAP_SLOW_PATH";
		if (!plus.get_pos ())
		  out << "0";
	      }
	    out << ");" << endl;
	    /* End if init_test call.  */

	    if (f.get_realign () && unaligned == 1)
	      out << "  __asm__ __volatile__ (\"subq $8,%%rsp\":::\"cc\");"
		  << endl;

	    out << "  ret = do_test_"
		<< (f.get_realign () && unaligned == 1 ? "u" : "")
		<< (f.get_varargs () ? "v" : "")
		<< fargs.size () << " (";

	    list_delimiter comma (", ");
	    for (auto const &arg : fargs)
	      out << comma.get () << arg.get_name ();
	    out << ");" << endl;

	    if (f.get_realign () && unaligned == 1)
	      out << "  __asm__ __volatile__ (\"addq $8,%%rsp\":::\"cc\");"
		  << endl;

	    out << "  check_results (ret);" << endl;
	  }
    }

  /* Close the last function and define the main do_tests function.  */
  out << "}" << endl << endl;

  /* Define _noinfo pointers to each do_tests_* function.  */
  for (auto const &fn_name : do_tests_fn_names)
    out << "  static void (*volatile " << fn_name << "_noinfo) ("
	<< param_types_str << ") = " << fn_name << ";" << endl;

  /* Define main do_tests () function.  */
  out << endl
      << "void do_tests ()" << endl
      << "{" << endl;
  i = 1;
  for (auto const &arg : args)
    {
      out << "  " << arg.get_type () << " " << arg.get_name () << " = " << i
	  << ";" << endl;
      i <<= 1;
    }
  out << endl;

  /* Call do_tests_*_noinfo functions.  */
  for (auto const &fn_name : do_tests_fn_names)
    out << "  " << fn_name << "_noinfo (" << param_names_str << ");" << endl;
  out << "}" << endl << endl;
}

/* Generate output file.  */
void generate_header (const string &args)
{
  vector<class arg> all_args;
  vector<vector<class arg> > arg_sets;

  ofstream out;
  out.exceptions (ios::failbit | ios::badbit);
  out.open (out_file_name);
  out << "/* Generated with " << args << " */" << endl << endl;

  assert (extra_params_max < 26);

  /* Build the extra argument array.  */
  for (unsigned int i = 0; i < extra_params_max; ++i)
    {
      char name[2] = "a";
      name[0] += i;
      class arg myarg (name, "long", true);

      all_args.push_back (myarg);
    }

  arg_sets.resize (extra_params_max - extra_params_min + 1);
  for (unsigned int i = 0; i < arg_sets.size (); ++i)
      arg_sets[i].insert (arg_sets[i].end(), all_args.begin(),
			  all_args.begin () + i + extra_params_min);

  /* Print sysv functions */
  for (const vector<class arg> &as : arg_sets)
    {
      const int alloca_max = !!(fn_variant_mask & FN_VAR_MSABI);
      const int varargs_max = !!(fn_variant_mask & FN_VAR_VARARGS);
      fn *fn;
      for (int _alloca = 0; _alloca <= alloca_max; ++_alloca)
	for (int varargs = 0; varargs <= varargs_max; ++varargs)
	{
	  try {
	    int var = (_alloca ? FN_VAR_ALLOCA : 0)
		    | (varargs ? FN_VAR_VARARGS : 0);
	    fn = new ::fn (as, 0, var);
	  } catch (invalid_argument) {
	    continue;
	  }
	  sysv_funcs.push_back (fn);
	  fn->print_def (out);
	}
    }

  /* Print _noinfo function pointers for sysv functions.  */
  for (const fn *f : sysv_funcs)
    f->print_noinfo_def (out);

  /* Print ms_abi functions.  */
  unsigned int var;
  for (var = 0; var <= FN_VAR_MASK; ++var)
    {
      /* We only want ms_abi fns for this.  */
      if (! (var & FN_VAR_MSABI))
	continue;

      /*  */
      if ((var & fn_variant_mask) != var)
	continue;

      unsigned clobbers;
      for (clobbers = 0; clobbers <= OPTIONAL_REG_ALL; ++clobbers)
	{
	  /* Skip clobbers that would be invalid.  */
	  if (clobbers & OPTIONAL_REG_RBP)
	    {
	      /* Whole program built with hard frame pointer.  */
	      if (omit_rbp_clobbers)
		continue;

	      /* Uses BP explicitly.  */
	      if (var & FN_VAR_HFP_OR_REALIGN)
		continue;

	      /* Alloca seems to require DRAP, which uses BP.  */
	      if (var & FN_VAR_ALLOCA)
		continue;
	    }

	  for (auto const &as : arg_sets)
	    {
	      fn *fn;
	      try {
		fn = new ::fn (as, clobbers, var);
	      } catch (invalid_argument) {
		continue;
	      }

	      msabi_funcs.push_back (fn);
	      fn->print_def (out);
	    }
	}
    }

  out << endl;
  make_do_tests_decl (all_args, out);
  out << endl;

  make_do_test (all_args, msabi_funcs, out);
  out.close ();
}

/* Parse a string into a long and return true upon success.  */
static bool long_optarg (const char *optarg, long &dest)
{
  char *end;

  errno = 0;
  dest = strtol(optarg, &end, 0);
  if (errno)
    cerr << strerror(errno) << endl;

  while (isspace(*end))
    ++end;

  /* Error if errno non-zero or junk at end of string.  */
  return errno || *end;
}

void usage ()
{
  cerr
<< "Usage: " << argv0 << " [options] <output_file>" << endl
<< endl
<< "    -p <n|n-n>, --max-extra-params <expr>" << endl
<< "        A single or range of extra parameters" << endl
<< "        Examples:" << endl
<< "            -p0-5" << endl
<< "            -p12" << endl
<< endl
<< "    -v <n>, --variant-mask <n>" << endl
<< "        Set mask of test variants (see enum fn_variants for values," << endl
<< "        defaults to 0x" << hex << FN_VAR_MASK << " [FN_VAR_MASK])" << endl
<< endl
<< "    -0, --omit-rbp-clobbers" << endl
<< "        Omit tests that clobber RBP." << endl;
  exit (-1);
}

/* Parse string representing a number range or a list of numbers.  */
void set_extra_param_counts (const char *str)
{
  char copy[0x40];
  char *max_str;
  bool bad = false;
  long int min, max;

  strncpy (copy, str, sizeof (copy) - 1);
  max_str = strchr(copy, '-');
  if (max_str)
      *max_str++ = 0;

  bad = long_optarg (copy, min);
  if (max_str)
    bad = bad || long_optarg (max_str, max);
  else
    max = min;

  if (min > max)
    usage ();

  extra_params_min = min;
  extra_params_max = max;
}

int main (int argc, char *argv[])
{
  argv0 = argv[0];
  const char *short_options = "p:v:0";
  const struct option long_options[] = {
    {"extra-params",		required_argument, 0, 'p'},
    {"variant-mask",		required_argument, 0, 'v'},
    {"omit-rbp-clobbers",	no_argument,	   0, '0'},
    {"help",			no_argument,	   0, 'h'},
    {0, 0, 0, 0},
  };

  int option_index = 0;
  int c;
  while ((c = getopt_long (argc, argv, short_options, long_options,
			   &option_index)) != -1)
    {
      switch (c)
	{
	long l;

	case 'p':
	  set_extra_param_counts (optarg);
	  break;

	case 'v':
	  if (long_optarg (optarg, l) || (l & ~FN_VAR_MASK))
	  {
	    cerr << "ERROR: Bad value for -v: `" << optarg <<  "`" << endl;
	    usage ();
	  }
	  fn_variant_mask = (unsigned)l;
	  break;

	case '0':
	  omit_rbp_clobbers = true;
	  break;

	case 'h':
	default:
	  usage ();
	}
    }

  if (argc - optind != 1)
    usage ();
  out_file_name = argv[optind];

  /* Can't skip msabi funcions.  */
  fn_variant_mask |= FN_VAR_MSABI;

  /* If whole program has HFP, explicit tests that enable it are redundant.  */
  if (omit_rbp_clobbers)
    fn_variant_mask &= ~FN_VAR_HFP;

  stringstream argv_str;

  for (int i = 0; i < argc; ++i)
    argv_str << (i ? " " : "") << argv[i];

  int ret = 0;
  try
    {
      generate_header (argv_str.str());
    }
  catch (exception &e)
    {
      cerr << "ERROR: While writing `" << out_file_name << "': "
	   << strerror(errno) << endl;
      ret = 1;
    }
  for_each (sysv_funcs.begin (), sysv_funcs.end (), default_delete<fn> ());
  for_each (msabi_funcs.begin (), msabi_funcs.end (), default_delete<fn> ());

  return ret;
}
