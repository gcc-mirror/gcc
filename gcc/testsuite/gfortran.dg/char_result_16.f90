! PR fortran/78757
! { dg-do compile }
! { dg-options "-O1" }

program pr78757
  implicit none
  character (len = 30), target :: x
  character (len = 30), pointer :: s
  s => foo (30_8)
contains
  function foo (i)
    integer (8) :: i
    character (len = i), pointer :: foo
    foo => x
  end function foo
end program pr78757
