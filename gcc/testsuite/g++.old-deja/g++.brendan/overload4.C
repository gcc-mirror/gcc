// { dg-do assemble  }
// GROUPS passed overloading
  typedef void *		(*NewObject) (void);
  
  class B
  {
  public:
	static void WantsNew (NewObject creator); // { dg-message "B::WantsNew|no known conversion" }
  };
  
  class A
  {
  public:
      static A * NewOne (void);
  
      static void InitClass (void)
      {
	  B::WantsNew ( (NewObject) A::NewOne );
	  // This used to die in convert_harshness_{ansi,old} cuz it
	  // didn't know what to do about a void type.
	  B::WantsNew ( A::NewOne );// { dg-error "no matching|conversion" }
      }
  };
