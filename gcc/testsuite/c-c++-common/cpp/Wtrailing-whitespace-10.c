/* This test uses intentionally DOS line endings (CR+LF).  */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-Wno-trailing-whitespace" } */

int i;   
int j;		
int \	
  k \	
  ;	
/* { dg-warning "backslash and newline separated by space" "" { target *-*-* } .-3 } */
/* { dg-warning "backslash and newline separated by space" "" { target *-*-* } .-3 } */

 
 

 
 
const char *p = R"*|*(		
  
        
.
)*|*";	 
// This is a comment with trailing whitespace 
/* This is a comment with trailing whitespace	
*/
// This is a comment with trailing whitespace 
/* This is a comment with trailing whitespace 
*/
    