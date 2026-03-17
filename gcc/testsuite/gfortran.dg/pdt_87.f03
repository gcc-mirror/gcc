! { dg-do compile }
!
! Test the fix for pr115316, in which none of the deferred type PDT errors were caught.
! Note the exclusion of the old-style character length at line 62. This compiles OK
! but causes an excess errors message in the testsuite.
!
! Contributed by David Binderman  <dcb314@hotmail.com>
!
subroutine C933_a(b1, ca3, ca4, cp3, cp3mold, cp4, cp7, cp8, bsrc)
! If any allocate-object has a deferred type parameter, is unlimited polymorphic,
! or is of abstract type, either type-spec or source-expr shall appear.
  type SomeType(k, l1, l2)
    integer, kind :: k = 1
    integer, len :: l1
    integer, len :: l2 = 3
    character(len=l2+l1) str
  end type

  type B(l)
    integer, len :: l
    character(:), allocatable :: msg
    type(SomeType(4, l, :)), pointer :: something
  end type
  character(len=:), allocatable :: ca1, ca2(:)
  character(len=*), allocatable :: ca3, ca4(:)
  character(len=2), allocatable :: ca5, ca6(:)
  character(len=5) mold

  type(SomeType(l1=:,l2=:)), pointer :: cp1, cp2(:)
  type(SomeType(l1=3,l2=4)) cp1mold
  type(SomeType(1,*,:)), pointer :: cp3, cp4(:)
  type(SomeType(1,*,5)) cp3mold
  type(SomeType(l1=:)), pointer :: cp5, cp6(:)
  type(SomeType(l1=6)) cp5mold
  type(SomeType(1,*,*)), pointer :: cp7, cp8(:)
  type(SomeType(1, l1=3)), pointer :: cp9, cp10(:)

  type(B(*)) b1
  type(B(:)), allocatable :: b2
  type(B(5)) b3

  type(SomeType(4, *, 8)) bsrc

  allocate(ca1)          ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(ca2(4))       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(cp1)          ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(cp2(2))       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(cp3)          ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(cp4(2))       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(cp5)          ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(cp6(2))       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(b1%msg)       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(b1%something) ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(b2%msg)       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(b2%something) ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(b3%msg)       ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" }
  allocate(b3%something) ! { dg-error "requires either a type-spec or SOURCE tag or a MOLD tag" } 

  ! Nominal cases, expecting no errors
  allocate(character(len=5):: ca2(4))
  allocate(character(len=5):: ca1)
!  allocate(character*5:: ca1)
  allocate(ca2(4), MOLD = "abcde")
  allocate(ca2(2), MOLD = (/"abcde", "fghij"/))
  allocate(ca1, MOLD = mold)
  allocate(ca2(4), SOURCE = "abcde")
  allocate(ca2(2), SOURCE = (/"abcde", "fghij"/))
  allocate(ca1, SOURCE = mold)
  allocate(SomeType(l1=1, l2=2):: cp1, cp2(2))
  allocate(SomeType(1,*,5):: cp3, cp4(2))
  allocate(SomeType(l1=1):: cp5, cp6(2))
  allocate(cp1, cp2(2), mold = cp1mold)
  allocate(cp3, cp4(2), mold = cp3mold)
  allocate(cp5, cp6(2), mold = cp5mold)
  allocate(cp1, cp2(2), source = cp1mold)
  allocate(cp3, cp4(2), source = cp3mold)
  allocate(cp5, cp6(2), source = cp5mold)
  allocate(character(len=10):: b1%msg, b2%msg, b3%msg)
  allocate(SomeType(4, b1%l, 9):: b1%something)
  allocate(b2%something, source=bsrc)
  allocate(SomeType(4, 5, 8):: b3%something)

  ! assumed/explicit length do not need type-spec/mold
  allocate(ca3, ca4(4))
  allocate(ca5, ca6(4))
  allocate(cp7, cp8(2))
  allocate(cp9, cp10(2))

end subroutine
