! { dg-do compile }
! { dg-options "-fdump-tree-original -Wc-binding-type" }
!
! PR fortran/34079
! Character bind(c) arguments shall not pass the length as additional argument
!

subroutine multiArgTest()
  implicit none
interface ! Array
  subroutine multiso_array(x,y) bind(c)
    use iso_c_binding
    character(kind=c_char,len=1), dimension(*) :: x,y
  end subroutine multiso_array
  subroutine multiso2_array(x,y) bind(c) ! { dg-warning "may not be C interoperable" }
    character(len=1), dimension(*) :: x,y
  end subroutine multiso2_array
  subroutine mult_array(x,y)
    use iso_c_binding
    character(kind=c_char,len=1), dimension(*) :: x,y
  end subroutine mult_array
end interface

interface ! Scalar: call by reference
  subroutine multiso(x,y) bind(c)
    use iso_c_binding
    character(kind=c_char,len=1) :: x,y
  end subroutine multiso
  subroutine multiso2(x,y) bind(c) ! { dg-warning "may not be C interoperable" }
    character(len=1) :: x,y
  end subroutine multiso2
  subroutine mult(x,y)
    use iso_c_binding
    character(kind=c_char,len=1) :: x,y
  end subroutine mult
end interface

interface ! Scalar: call by VALUE
  subroutine multiso_val(x,y) bind(c)
    use iso_c_binding
    character(kind=c_char,len=1), value :: x,y
  end subroutine multiso_val
  subroutine multiso2_val(x,y) bind(c) ! { dg-warning "may not be C interoperable" }
    character(len=1), value :: x,y
  end subroutine multiso2_val
  subroutine mult_val(x,y)
    use iso_c_binding
    character(kind=c_char,len=1), value :: x,y
  end subroutine mult_val
end interface

call mult_array    ("abc","ab")
call multiso_array ("ABCDEF","ab")
call multiso2_array("AbCdEfGhIj","ab")

call mult    ("u","x")
call multiso ("v","x")
call multiso2("w","x")

call mult_val    ("x","x")
call multiso_val ("y","x")
call multiso2_val("z","x")
end subroutine multiArgTest

program test
implicit none

interface ! Array
  subroutine subiso_array(x) bind(c)
    use iso_c_binding
    character(kind=c_char,len=1), dimension(*) :: x
  end subroutine subiso_array
  subroutine subiso2_array(x) bind(c) ! { dg-warning "may not be C interoperable" }
    character(len=1), dimension(*) :: x
  end subroutine subiso2_array
  subroutine sub_array(x)
    use iso_c_binding
    character(kind=c_char,len=1), dimension(*) :: x
  end subroutine sub_array
end interface

interface ! Scalar: call by reference
  subroutine subiso(x) bind(c)
    use iso_c_binding
    character(kind=c_char,len=1) :: x
  end subroutine subiso
  subroutine subiso2(x) bind(c) ! { dg-warning "may not be C interoperable" }
    character(len=1) :: x
  end subroutine subiso2
  subroutine sub(x)
    use iso_c_binding
    character(kind=c_char,len=1) :: x
  end subroutine sub
end interface

interface ! Scalar: call by VALUE
  subroutine subiso_val(x) bind(c)
    use iso_c_binding
    character(kind=c_char,len=1), value :: x
  end subroutine subiso_val
  subroutine subiso2_val(x) bind(c) ! { dg-warning "may not be C interoperable" }
    character(len=1), value :: x
  end subroutine subiso2_val
  subroutine sub_val(x)
    use iso_c_binding
    character(kind=c_char,len=1), value :: x
  end subroutine sub_val
end interface

call sub_array    ("abc")
call subiso_array ("ABCDEF")
call subiso2_array("AbCdEfGhIj")

call sub    ("u")
call subiso ("v")
call subiso2("w")

call sub_val    ("x")
call subiso_val ("y")
call subiso2_val("z")
end program test

! Double argument dump:
!
! { dg-final { scan-tree-dump "mult_array .&.abc..1..lb: 1 sz: 1., &.ab..1..lb: 1 sz: 1., 3, 2.;" "original" } }
! { dg-final { scan-tree-dump "multiso_array .&.ABCDEF..1..lb: 1 sz: 1., &.ab..1..lb: 1 sz: 1..;" "original" } }
! { dg-final { scan-tree-dump "multiso2_array .&.AbCdEfGhIj..1..lb: 1 sz: 1., &.ab..1..lb: 1 sz: 1..;" "original" } }
!
! { dg-final { scan-tree-dump "mult .&.u..1..lb: 1 sz: 1., &.x..1..lb: 1 sz: 1., 1, 1.;" "original" } }
! { dg-final { scan-tree-dump "multiso .&.v..1..lb: 1 sz: 1., &.x..1..lb: 1 sz: 1..;" "original" } }
! { dg-final { scan-tree-dump "multiso2 .&.w..1..lb: 1 sz: 1., &.x..1..lb: 1 sz: 1..;" "original" } }
!
! { dg-final { scan-tree-dump "mult_val ..x., .x., 1, 1.;" "original" } }
! { dg-final { scan-tree-dump "multiso_val .121, 120.;" "original" } }
! { dg-final { scan-tree-dump "multiso2_val ..z., .x..;" "original" } }
!
! Single argument dump:
!
! { dg-final { scan-tree-dump "sub_array .&.abc..1..lb: 1 sz: 1., 3.;" "original" } }
! { dg-final { scan-tree-dump "subiso_array .&.ABCDEF..1..lb: 1 sz: 1..;" "original" } }
! { dg-final { scan-tree-dump "subiso2_array .&.AbCdEfGhIj..1..lb: 1 sz: 1..;" "original" } }
!
! { dg-final { scan-tree-dump "sub .&.u..1..lb: 1 sz: 1., 1.;" "original" } }
! { dg-final { scan-tree-dump "subiso .&.v..1..lb: 1 sz: 1..;" "original" } }
! { dg-final { scan-tree-dump "subiso2 .&.w..1..lb: 1 sz: 1..;" "original" } }
!
! { dg-final { scan-tree-dump "sub_val ..x., 1.;" "original" } }
! { dg-final { scan-tree-dump "subiso_val .121.;" "original" } }
! { dg-final { scan-tree-dump "subiso2_val ..z..;" "original" } }
!
