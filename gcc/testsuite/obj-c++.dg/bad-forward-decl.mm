class TestCPP { }; 	/* { dg-error "previous declaration of" } */

@class TestCPP;		/* { dg-error "redeclared as different kind of symbol" } */
