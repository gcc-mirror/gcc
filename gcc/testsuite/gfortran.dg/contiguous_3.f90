! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/40632
!
! CONTIGUOUS compile-time tests: Check that contigous
! works properly.

subroutine test1(a,b)
  integer, pointer, contiguous :: test1_a(:)
  call foo(test1_a)
  call foo(test1_a(::1))
  call foo(test1_a(::2))
contains
  subroutine foo(b)
    integer :: b(*)
  end subroutine foo
end subroutine test1

! For the first two no pack is done; for the third one, an array descriptor
! (cf. below test3) is created for packing.
!
! { dg-final { scan-tree-dump-times "_internal_pack.*test1_a" 0 "original" } }
! { dg-final { scan-tree-dump-times "_internal_unpack.*test1_a" 0 "original" } }


subroutine t2(a1,b1,c2,d2)
  integer, pointer, contiguous :: a1(:), b1(:)
  integer, pointer :: c2(:), d2(:)
  a1 = b1
  c2 = d2
end subroutine t2

! { dg-final { scan-tree-dump-times "= a1->dim.0..stride;" 0 "original" } }
! { dg-final { scan-tree-dump-times "= b1->dim.0..stride;" 0 "original" } }
! { dg-final { scan-tree-dump-times "= c2->dim.0..stride;" 1 "original" } }
! { dg-final { scan-tree-dump-times "= d2->dim.0..stride;" 1 "original" } }


subroutine test3()
  implicit none
  integer :: test3_a(8),i
  test3_a = [(i,i=1,8)]
  call foo(test3_a(::1))
  call foo(test3_a(::2))
  call bar(test3_a(::1))
  call bar(test3_a(::2))
contains
  subroutine foo(x)
    integer, contiguous :: x(:)
    print *, x
  end subroutine
  subroutine bar(x)
    integer :: x(:)
    print *, x
  end subroutine bar
end subroutine test3

! Once for test1 (third call), once for test3 (second call)
! { dg-final { scan-tree-dump-times "data = origptr" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_internal_pack .&parm" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_internal_unpack .&parm" 2 "original" } }


