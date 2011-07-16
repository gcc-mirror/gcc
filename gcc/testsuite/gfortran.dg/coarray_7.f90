! { dg-do compile }
! { dg-options "-fmax-errors=1000 -fcoarray=single" }
!
! PR fortran/18918
!
! Coarray expressions.
!
program test
  implicit none
  type t3
    integer, allocatable :: a
  end type t3
  type t4
    type(t3) :: xt3
  end type t4
  type t
    integer, pointer :: ptr
    integer, allocatable :: alloc(:)
  end type t
  type(t), target :: i[*]
  type(t), allocatable :: ca[:]
  type(t4), target :: tt4[*]
  type(t4), allocatable :: ca2[:]
  integer, volatile :: volat[*]
  integer, asynchronous :: async[*]
  integer :: caf1[1,*], caf2[*]
  allocate(i%ptr)
  call foo(i%ptr)
  call foo(i[1]%ptr) ! { dg-error "Coindexed actual argument at .1. to pointer dummy" }
  call bar(i%ptr)
  call bar(i[1]%ptr) ! OK, value of ptr target 
  call bar(i[1]%alloc(1)) ! OK
  call typeDummy(i) ! OK
  call typeDummy(i[1]) ! { dg-error "with ultimate pointer component" }
  call typeDummy2(ca) ! OK
  call typeDummy2(ca[1]) ! { dg-error "with ultimate pointer component" }
  call typeDummy3(tt4%xt3) ! OK
  call typeDummy3(tt4[1]%xt3) ! { dg-error "requires either VALUE or INTENT.IN." }
  call typeDummy4(ca2) ! OK
  call typeDummy4(ca2[1]) ! { dg-error "requires INTENT.IN." }
! Note: Checking an VOLATILE dummy is not possible as volatile + intent(in)
! is not possible

  call asyn(volat)
  call asyn(async)
  call asyn(volat[1]) ! { dg-error "Coindexed ASYNCHRONOUS or VOLATILE actual argument" }
  call asyn(async[1]) ! { dg-error "Coindexed ASYNCHRONOUS or VOLATILE actual argument" }

  call coarray(caf1) ! rank mismatch; OK, for non allocatable coarrays
  call coarray(caf2)
  call coarray(caf2[1]) ! { dg-error "must be a coarray" }
  call ups(i)
  call ups(i[1]) ! { dg-error "with ultimate pointer component" }
  call ups(i%ptr)
  call ups(i[1]%ptr) ! OK - passes target not pointer
contains
  subroutine asyn(a)
    integer, intent(in), asynchronous :: a
  end subroutine asyn
  subroutine bar(a)
    integer :: a
  end subroutine bar
  subroutine foo(a)
    integer, pointer :: a
  end subroutine foo
  subroutine coarray(a)
    integer :: a[*]
  end subroutine coarray
  subroutine typeDummy(a)
    type(t) :: a
  end subroutine typeDummy
  subroutine typeDummy2(a)
    type(t),allocatable :: a
  end subroutine typeDummy2
  subroutine typeDummy3(a)
    type(t3) :: a
  end subroutine typeDummy3
  subroutine typeDummy4(a)
    type(t4), allocatable :: a
  end subroutine typeDummy4
end program test


subroutine alloc()
type t
  integer, allocatable :: a(:)
end type t
type(t), save :: a[*]
type(t), allocatable :: b(:)[:], C[:]

allocate(b(1)) ! { dg-error "Coarray specification" }
allocate(a[3]%a(5)) ! { dg-error "Coindexed allocatable" }
allocate(c[*]) ! OK
allocate(a%a(5)) ! OK
end subroutine alloc


subroutine dataPtr()
  integer, save, target :: a[*]
  data a/5/ ! OK
  data a[1]/5/ ! { dg-error "cannot have a coindex" }
  type t
  integer, pointer :: p
  end type t
  type(t), save :: x[*]

  type t2
    integer :: a(1)
  end type t2
  type(t2) y
  data y%a/4/


   x[1]%p => a  ! { dg-error "shall not have a coindex" }
   x%p => a[1]  ! { dg-error "shall not have a coindex" }
end subroutine dataPtr


subroutine test3()
implicit none
type t
  integer :: a(1)
end type t
type(t), save :: x[*]
data x%a/4/

  integer, save :: y(1)[*] !(1)
  call sub(x(1:1)[1]) ! { dg-error "Rank mismatch" }
contains
  subroutine sub(a) ! { dg-error "shall not have codimensions with deferred shape" }
    integer :: a(:)[:]
  end subroutine sub
end subroutine test3


subroutine test4()
  integer, save :: i[*]
  integer :: j
  call foo(i)
  call foo(j) ! { dg-error "must be a coarray" }
contains
  subroutine foo(a)
    integer :: a[*]
  end subroutine foo
end subroutine test4


subroutine allocateTest()
  implicit none
  real, allocatable, codimension[:,:] :: a,b,c
  integer :: n, q
  n = 1
  q = 1
  allocate(a[q,*]) ! OK
  allocate(b[q,*]) ! OK
  allocate(c[q,*]) ! OK
end subroutine allocateTest


subroutine testAlloc4()
  implicit none
  type co_double_3
    double precision, allocatable :: array(:)
  end type co_double_3
  type(co_double_3),save, codimension[*] :: work
  allocate(work%array(1))
  print *, size(work%array)
end subroutine testAlloc4

subroutine test5()
  implicit none
  integer, save :: i[*]
  print *, i[*] ! { dg-error "Coindex of codimension 1 must be a scalar" }
end subroutine test5

