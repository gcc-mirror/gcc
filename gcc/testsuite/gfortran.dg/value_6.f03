! { dg-do run }
! Verify by-value passing of character arguments w/in Fortran to a bind(c) 
! procedure.  
! PR fortran/32732
module pr32732
  use, intrinsic :: iso_c_binding, only: c_char
  implicit none
contains
  subroutine test(a) bind(c)
    character(kind=c_char), value :: a
    call test2(a)
  end subroutine test
  subroutine test2(a) bind(c)
    character(kind=c_char), value :: a
    if(a /= c_char_'a') call abort ()
    print *, 'a=',a
  end subroutine test2
end module pr32732

program main
  use pr32732
  implicit none
  call test('a')
end program main
! { dg-final { cleanup-modules "pr32732" } }
