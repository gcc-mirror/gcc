/* { dg-do compile } */

#pragma GCC push_options
#pragma GCC optimize ("-fno-lto") /* { dg-warning "bad option" } */
int main(void){return 0;}
#pragma GCC pop_options

/* ???  The FEs still apply the pragma string as optimize attribute to
   all functions thus the diagnostic will be repeated for each function
   affected.  */
/* { dg-message "bad option" "" { target *-*-* } 0 } */
