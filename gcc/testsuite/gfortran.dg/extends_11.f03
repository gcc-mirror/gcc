! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/45586
! Test that access to inherited components are properly generated
!
! Stripped down from extends_1.f03
!
  type :: person
    integer :: ss = 1
  end type person

  type, extends(person) :: education
    integer ::  attainment = 0
  end type education

  type, extends(education) :: service
    integer :: personnel_number = 0
  end type service

  type, extends(service) :: person_record
    type (person_record), pointer :: supervisor => NULL ()
  end type person_record
  
  type(person_record) :: recruit
  

  ! Check that references by ultimate component and by parent type work
  ! All the following component access are equivalent
  recruit%ss = 2
  recruit%person%ss = 3
  recruit%education%ss = 4
  recruit%education%person%ss = 5
  recruit%service%ss = 6
  recruit%service%person%ss = 7
  recruit%service%education%ss = 8
  recruit%service%education%person%ss = 9
end

! { dg-final { scan-tree-dump-times " +recruit\\.service\\.education\\.person\\.ss =" 8 "original"} }
! { dg-final { cleanup-tree-dump "original" } }
