! Program to test the PRESENT intrinsic
program intrinsic_present
   implicit none
   integer a
   integer, pointer :: b
   integer, dimension(10) :: c
   integer, pointer, dimension(:) :: d
   
   if (testvar()) call abort ()
   if (.not. testvar(a)) call abort ()
   if (testptr()) call abort ()
   if (.not. testptr(b)) call abort ()
   if (testarray()) call abort ()
   if (.not. testarray(c)) call abort ()
   if (testparray()) call abort ()
   if (.not. testparray(d)) call abort ()
   
contains
logical function testvar (p)
   integer, optional :: p
   testvar = present(p)
end function

logical function testptr (p)
   integer, pointer, optional :: p
   testptr = present(p)
end function

logical function testarray (p)
   integer, dimension (10), optional :: p
   testarray = present(p)
end function

logical function testparray (p)
   integer, pointer, dimension(:), optional :: p
   testparray = present(p)
end function

end program

