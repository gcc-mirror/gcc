! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/40632
!
! CONTIGUOUS compile-time tests
!

! C448: Must be an array with POINTER attribute
type t1
  integer, contiguous :: ca(5) ! { dg-error "Component .ca. at .1. has the CONTIGUOUS" }
end type t1
type t2
  integer, contiguous, allocatable :: cb(:) ! { dg-error "Component .cb. at .1. has the CONTIGUOUS" }
end type t2
type t3
  integer, contiguous, pointer :: cc(:) ! OK
end type t3
type t4
  integer, pointer, contiguous :: cd ! { dg-error "Component .cd. at .1. has the CONTIGUOUS" }
end type t4
end

! C530: Must be an array and (a) a POINTER or (b) assumed shape.
subroutine test(x, y)
  integer, pointer :: x(:)
  integer, intent(in) :: y(:)
  contiguous :: x, y

  integer, contiguous :: a(5) ! { dg-error ".a. at .1. has the CONTIGUOUS attribute" }
  integer, contiguous, allocatable :: b(:) ! { dg-error ".b. at .1. has the CONTIGUOUS attribute" }
  integer, contiguous, pointer :: c(:) ! OK
  integer, pointer, contiguous :: d ! { dg-error ".d. at .1. has the CONTIGUOUS attribute" }
end

! Pointer assignment check:
! If the pointer object has the CONTIGUOUS attribute, the pointer target shall be contiguous.
! Note: This is not compile-time checkable; but F2008, 5.3.7 except in a very few cases.
subroutine ptr_assign()
  integer, pointer, contiguous :: ptr1(:)
  integer, target :: tgt(5)
  ptr1 => tgt
end subroutine


! C1239 (R1223) If an actual argument is a nonpointer array that has the ASYNCHRONOUS or VOLATILE
! attribute but is not simply contiguous (6.5.4), and the corresponding dummy argument has either the
! VOLATILE or ASYNCHRONOUS attribute, that dummy argument shall be an assumed-shape array
! that does not have the CONTIGUOUS attribute.

subroutine C1239
  type t
    integer :: e(4)
  end type t
  type(t), volatile :: f
  integer, asynchronous :: a(4), b(4)
  integer, volatile :: c(4), d(4)
  call test (a,b,c)      ! OK
  call test (a,b(::2),c) ! { dg-error "array without CONTIGUOUS" }
  call test (a(::2),b,c) ! { dg-error "array without CONTIGUOUS" }

  call test (a,b,f%e)      ! OK
  call test (a,f%e,c)      ! OK
  call test (f%e,b,c)      ! OK
  call test (a,b,f%e(::2)) ! OK
  call test (a,f%e(::2),c) ! { dg-error "array without CONTIGUOUS" }
  call test (f%e(::2),b,c) ! { dg-error "array without CONTIGUOUS" }
contains
  subroutine test(u, v, w)
    integer, asynchronous :: u(:), v(*)
    integer, volatile :: w(:)
    contiguous :: u
  end subroutine test
end subroutine C1239


! C1240 (R1223) If an actual argument is an array pointer that has the ASYNCHRONOUS or VOLATILE
! attribute but does not have the CONTIGUOUS attribute, and the corresponding dummy argument has
! either the VOLATILE or ASYNCHRONOUS attribute, that dummy argument shall be an array pointer
! or an assumed-shape array that does not have the CONTIGUOUS attribute.

subroutine C1240
  type t
    integer,pointer :: e(:)
  end type t
  type(t), volatile :: f
  integer, pointer, asynchronous :: a(:), b(:)
  integer,pointer, volatile :: c(:), d(:)
  call test (a,b,c)      ! { dg-error "array without CONTIGUOUS" }
  call test (a,b(::2),c) ! { dg-error "array without CONTIGUOUS" }
  call test (a(::2),b,c) ! { dg-error "array without CONTIGUOUS" }

  call test (a,b,f%e)      ! { dg-error "array without CONTIGUOUS" }
  call test (a,f%e,c)      ! { dg-error "array without CONTIGUOUS" }
  call test (f%e,b,c)      ! { dg-error "array without CONTIGUOUS" }
  call test (a,b,f%e(::2)) ! { dg-error "array without CONTIGUOUS" }
  call test (a,f%e(::2),c) ! { dg-error "array without CONTIGUOUS" }
  call test (f%e(::2),b,c) ! { dg-error "array without CONTIGUOUS" }

  call test2(a,b)
  call test3(a,b)
  call test2(c,d)
  call test3(c,d)
  call test2(f%e,d)
  call test3(c,f%e)
contains
  subroutine test(u, v, w)
    integer, asynchronous :: u(:), v(*)
    integer, volatile :: w(:)
    contiguous :: u
  end subroutine test
  subroutine test2(x,y)
    integer, asynchronous :: x(:)
    integer, volatile :: y(:)
  end subroutine test2 
  subroutine test3(x,y)
    integer, pointer, asynchronous :: x(:)
    integer, pointer, volatile :: y(:)
  end subroutine test3
end subroutine C1240



! 12.5.2.7 Pointer dummy variables
! C1241 The actual argument corresponding to a dummy pointer with the CONTIGUOUS attribute shall be
! simply contiguous (6.5.4).

subroutine C1241
  integer, pointer, contiguous :: a(:)
  integer, pointer :: b(:)
  call test(a)
  call test(b) ! { dg-error "must be simply contiguous" }
contains
  subroutine test(x)
    integer, pointer, contiguous :: x(:)
  end subroutine test
end subroutine C1241


! 12.5.2.8 Coarray dummy variables
! If the dummy argument is an array coarray that has the CONTIGUOUS attribute or is not of assumed shape,
! the corresponding actual argument shall be simply contiguous

subroutine sect12528(cob)
  integer, save :: coa(6)[*]
  integer :: cob(:)[*]

  call test(coa)
  call test2(coa)
  call test3(coa)

  call test(cob) ! { dg-error "must be simply contiguous" }
  call test2(cob) ! { dg-error "must be simply contiguous" }
  call test3(cob)
contains
  subroutine test(x)
    integer, contiguous :: x(:)[*]
  end subroutine test
  subroutine test2(x)
    integer :: x(*)[*]
  end subroutine test2
  subroutine test3(x)
    integer :: x(:)[*]
  end subroutine test3
end subroutine sect12528



subroutine test34
  implicit none
  integer, volatile,pointer :: a(:,:),i
  call foo(a(2,2:3:2)) ! { dg-error "must be simply contiguous" }
contains
  subroutine foo(x)
    integer, pointer, contiguous, volatile :: x(:)
  end subroutine
end subroutine test34
