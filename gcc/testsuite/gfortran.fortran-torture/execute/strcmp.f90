program test
   implicit none
   character(len=20) :: foo

   foo="hello"

   if (llt(foo, "hello")) call abort
   if (.not. lle(foo, "hello")) call abort
   if (lgt("hello", foo)) call abort
   if (.not. lge("hello", foo)) call abort

   if (.not. llt(foo, "world")) call abort
   if (.not. lle(foo, "world")) call abort
   if (lgt(foo, "world")) call abort
   if (lge(foo, "world")) call abort
end
