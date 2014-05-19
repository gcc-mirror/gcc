// PR c++/20536
// Found by Wolfgang Wieser 03/2005.
// { dg-do compile }
 
struct yyguts_t 
{ 
  class TestScanner* yyextra_r; // { dg-message "forward declaration" }
}; 
    
TestScanner::TestScanner() {} // { dg-error "invalid use" }
