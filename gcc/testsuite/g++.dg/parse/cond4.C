// PR c++/38635
// { dg-do compile }

void foo()                                                                                                                              
{                                                                                                                                       
  if (struct A{}// { dg-error "types may not be defined|expected" }
