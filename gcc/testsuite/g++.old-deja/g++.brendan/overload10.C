// { dg-do assemble  }
// GROUPS passed overloading
class Bed {
   public:
   static void bed_func(
      int        (*f)(int &, int, int));
};
class g_func {
public:
	static int save_status;

	// in compute_harshness, we should be using comptypes, not ==, to
	// check if this is equivalent to the previous decl; the only
	// difference is the default arg
	static int rpt_func(int &status, int expand, 
		int restore_cursor=1 );
};

int  main (int argc, 
           char **argv,
           char  **envp)
{
   Bed::bed_func(g_func::rpt_func);
   return(1);
}
