program test
   implicit none
   character(len=20) :: foo

   foo="hello"

   if (llt(foo, "hello")) STOP 1
   if (.not. lle(foo, "hello")) STOP 2
   if (lgt("hello", foo)) STOP 3
   if (.not. lge("hello", foo)) STOP 4

   if (.not. llt(foo, "world")) STOP 5
   if (.not. lle(foo, "world")) STOP 6
   if (lgt(foo, "world")) STOP 7
   if (lge(foo, "world")) STOP 8
end
