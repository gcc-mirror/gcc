/* This test uses intentionally DOS line endings (CR+LF).  */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-Wtrailing-whitespace=blanks" } */

int i;   
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-1 } */
int j;		
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-1 } */
int \	
  k \	
  ;	
/* { dg-warning "backslash and newline separated by space" "" { target *-*-* } .-3 } */
/* { dg-warning "backslash and newline separated by space" "" { target *-*-* } .-3 } */
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-3 } */

 
 
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-1 } */

 
 
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-1 } */
const char *p = R"*|*(		
  
        
.
)*|*";	 
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-1 } */
// This is a comment with trailing whitespace 
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-1 } */
/* This is a comment with trailing whitespace	
*/
/* { dg-warning "trailing whitespace" "" { target *-*-* } .-2 } */
// This is a comment with trailing whitespace 
/* This is a comment with trailing whitespace 
*/
/* { dg-warning "trailing whitespace" "" { target *-*-* } .+1 } */
    