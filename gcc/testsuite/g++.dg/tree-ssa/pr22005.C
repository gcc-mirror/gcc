/* { dg-do compile } */
/* { dg-options "-O2" } */
struct cl_string 
{
  union{ int i; };
  cl_string ();
};
struct cl_print_univpoly_flags { cl_string univpoly_varname; };
struct cl_print_flags: cl_print_univpoly_flags {int i;};
cl_print_flags default_print_flags;
