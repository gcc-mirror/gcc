! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for the 14/15 regression PR118750
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
  implicit none

  type string_t
    character(:), allocatable :: str
  end type

  associate(str_a => get_string([string_t ("abcd"),string_t ("ef")]))
    if (str_a(1)%str//str_a(2)%str /= "abcdef") STOP 1 ! Returned "Invalid array reference at (1)"
  end associate

contains

  type(string_t) elemental function get_string(mold)
    class(string_t), intent(in) :: mold
    get_string = string_t(mold%str)
  end function

end 
! { dg-final { scan-tree-dump-times "array01_string_t str_a" 1 "original" } }
