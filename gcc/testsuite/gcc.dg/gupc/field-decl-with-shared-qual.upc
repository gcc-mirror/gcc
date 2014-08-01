/* Field declared with UPC shared qualifier.  
	{ dg-do compile }
	{ dg-options "-fupc-threads=2" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



struct S_struct
  {
    int field1;
    shared double field2; /* { dg-error "field 'field2' declared with UPC shared qualifier" } */
    char field3;
  };
