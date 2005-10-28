// { dg-do compile }
// Make sure that the type of f1 does not change 
// after the error of main about not returning 
// int.
// From Pekka Vuorela <pvuorela@iki.fi> 
// PR c++/23229

void f1();  
  
void
main()  /* { dg-error "must return" } */
{     
  f1();  
}  
  
void f1()  
{     
}  

