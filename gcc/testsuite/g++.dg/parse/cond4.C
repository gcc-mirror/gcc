// PR c++/38635
// { dg-do compile }

void foo()                                                                                                                              
{                                                                                                                                       
  if (struct A{}// { dg-error "" }
