! { dg-do compile }
!
program foo

   use iso_c_binding, only: c_int, c_char, c_sizeof

   integer(kind=c_int) :: i

   character(kind=c_char,len=1),parameter :: str2(4) = ["a","b","c","d"]

   i = c_sizeof(str2(1:3)) ! { dg-error "must be an interoperable data" }

   if (i /= 3) call abort()

end program foo

