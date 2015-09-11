/* { dg-do compile } */
extern "C"
{
  typedef struct _IO_FILE FILE;
  extern int fprintf (FILE * __restrict __stream,
		      const char *__restrict __format, ...);
}
typedef union tree_node *tree;
class ipa_polymorphic_call_context
{
};
class ipcp_value_base
{
};
template < typename valtype > class ipcp_value:public ipcp_value_base
{
public:valtype value;
  ipcp_value *next;
};

template < typename valtype > class ipcp_lattice
{
public:ipcp_value < valtype > *values;
  void print (FILE * f, bool dump_sources, bool dump_benefits);
};

class ipcp_param_lattices
{
public:ipcp_lattice < tree > itself;
  ipcp_lattice < ipa_polymorphic_call_context > ctxlat;
};
template < typename valtype > void ipcp_lattice < valtype >::print (FILE * f,
								    bool
								    dump_sources,
								    bool
								    dump_benefits)
{
  ipcp_value < valtype > *val;
  bool prev = false;
  for (val = values; val; val = val->next)
    {
      if (dump_benefits && prev)
	fprintf (f, "               ");
      else if (!dump_benefits && prev)
	fprintf (f, ", ");
      else
	prev = true;
      if (dump_sources)
	fprintf (f, "]");
      if (dump_benefits)
	fprintf (f, "shit");
    }
}

void
print_all_lattices (FILE * f, bool dump_sources, bool dump_benefits)
{
  struct ipcp_param_lattices *plats;
  plats->ctxlat.print (f, dump_sources, dump_benefits);
}
