// { dg-do assemble  }
// g++ 1.37.1 bug 900514_03

// g++ fails to flag ERRORs on the following erroneous code.

// In Section 12.3.2 it says "Defining conversion by both a constructor and
// a conversion function can lead to ambiguities."  However in the case below,
// the explicit cast syntax disambiguates the constructor as one which
// invokes the type conversion operator rather than the conversion.

// NO, IT DOESN'T.  It's still ambiguous.  --jason 2002-12-03

// cfront 2.0 passes this test.

// keywords: user-defined type conversion operator, constructor

struct t_0_st_0;

struct t_0_st_1 {
  int member;

  t_0_st_1 (t_0_st_0&);// { dg-error "" } 
  t_0_st_1 ();
};

struct t_0_st_0 {
  int member;

  operator t_0_st_1 ();// { dg-error "" } 
};

t_0_st_0 t_0_st_0_obj0;

void t_0_assignment ()
{
  t_0_st_1 t_0_st_1_obj0;
  t_0_st_1 t_0_st_1_obj1;
  t_0_st_1 t_0_st_1_obj2;

  t_0_st_1_obj0 = t_0_st_0_obj0;			// { dg-error "" } caught
  t_0_st_1_obj1 = t_0_st_1 (t_0_st_0_obj0);
}

void t_0_local_init ()
{
  t_0_st_1 t_0_st_1_obj0 = t_0_st_0_obj0;		// { dg-error "" } 
  t_0_st_1 t_0_st_1_obj1 = t_0_st_1 (t_0_st_0_obj0);
}

struct t_1_st_0;

struct t_1_st_1 {
  int member;

  t_1_st_1 (t_1_st_0&);					// { dg-error "" } 
  t_1_st_1 ();
  void operator= (t_1_st_1&);				// { dg-error "" } 
};

struct t_1_st_0 {
  int member;

  operator t_1_st_1 ();					// { dg-error "" } 
};

t_1_st_0 t_1_st_0_obj0;

void t_1_assignment ()
{
  t_1_st_1 t_1_st_1_obj0;
  t_1_st_1 t_1_st_1_obj1;
  t_1_st_1 t_1_st_1_obj2;

  t_1_st_1_obj0 = t_1_st_0_obj0;			// { dg-error "" } 
  t_1_st_1_obj1 = t_1_st_1 (t_1_st_0_obj0);		// { dg-error "" } 
}

void t_1_local_init ()
{
  t_1_st_1 t_1_st_1_obj0 = t_1_st_0_obj0;		// { dg-error "" } 
  t_1_st_1 t_1_st_1_obj1 = t_1_st_1 (t_1_st_0_obj0);
}

struct t_2_st_0;

struct t_2_st_1 {
  int member;

  t_2_st_1 (t_2_st_0);		// { dg-error "" } candidate
  t_2_st_1 ();
};

struct t_2_st_0 {
  int member;

  operator t_2_st_1 ();		// { dg-error "" } candidate
};

t_2_st_0 t_2_st_0_obj0;

void t_2_assignment ()
{
  t_2_st_1 t_2_st_1_obj0;
  t_2_st_1 t_2_st_1_obj1;
  t_2_st_1 t_2_st_1_obj2;

  t_2_st_1_obj0 = t_2_st_0_obj0;			// { dg-error "" } caught
  t_2_st_1_obj1 = t_2_st_1 (t_2_st_0_obj0);
}

void t_2_local_init ()
{
  t_2_st_1 t_2_st_1_obj0 = t_2_st_0_obj0;		// { dg-error "" } 
  t_2_st_1 t_2_st_1_obj1 = t_2_st_1 (t_2_st_0_obj0);
}
