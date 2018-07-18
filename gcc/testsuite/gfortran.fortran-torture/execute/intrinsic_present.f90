! Program to test the PRESENT intrinsic
program intrinsic_present
   implicit none
   integer a
   integer, pointer :: b
   integer, dimension(10) :: c
   integer, pointer, dimension(:) :: d
   
   if (testvar()) STOP 1
   if (.not. testvar(a)) STOP 2
   if (testptr()) STOP 3
   if (.not. testptr(b)) STOP 4
   if (testarray()) STOP 5
   if (.not. testarray(c)) STOP 6
   if (testparray()) STOP 7
   if (.not. testparray(d)) STOP 8
   
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

