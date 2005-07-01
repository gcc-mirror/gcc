/* { dg-do compile } */
/* { dg-options "-O2" } */

/* This code ends up taking the address of part of the structure that is padding, 
   and because there is no real field there, the structure alias analyzer would 
   abort.  */
struct empty_class {};
struct class1 : empty_class
{
  class1() {}
  empty_class value_;
};
struct lambda : class1 { };
lambda _1;
