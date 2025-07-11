! { dg-do compile }
!
! Tests the variants of IMPORT introduced in F2018
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
MODULE M
  import, none          ! { dg-error "F2018: C897 IMPORT statement" }
  IMPLICIT NONE
  integer :: z
end module

MODULE N
  IMPLICIT NONE
  integer :: z
end module

! Taken from gfortran.dg/pr103312.f90. These F2008-style invocations should
! be accepted.
module example
  type, abstract :: foo
    integer :: i
  contains
    procedure(foo_size), deferred :: size
    procedure(foo_func), deferred :: func
  end type
  abstract interface
    pure integer function foo_size (this)
      import :: foo
      class(foo), intent(in) :: this
    end function
    function foo_func (this) result (string)
      import :: foo
      class(foo) :: this
      character(this%size()) :: string
    end function
  end interface
end module

block data blk
  import, all          ! { dg-error "F2018: C897 IMPORT statement" }
  integer a(2)
  common /my_common/a
  data a/1,2/
end

subroutine extern_sub1
  import               ! { dg-error "F2018: C897 IMPORT statement" }
end

subroutine extern_sub2 (arg1, arg2, arg3)
  implicit none
  integer :: arg1, arg2, arg3
  arg1 = int_fcn ()
contains
  integer function int_fcn () 
    import, only : arg2, arg3
    int_fcn = arg2 * arg3
  end
end

program p
  import, all          ! { dg-error "F2018: C897 IMPORT statement" }
  implicit none
  integer :: x, y
  type :: t
    integer :: i
  end type
  type(t) :: progtype
  type, extends(t) :: s
    integer :: j
  end type
  class(t), allocatable :: progclass
contains

! OK because arg is just that and x is declared in scope of sub1.
  subroutine sub1 (arg)
    import, none
    implicit none
    real :: arg, x
  end

! IMPORT, ALL must be the only IMPORT statement in the scope.
  subroutine sub2 (arg)
    import, none
    import, all         ! { dg-error "F2018: C8100 IMPORT statement" }
    implicit none
    real :: arg, x
  end

! Error message says it all.
  subroutine sub3 (arg)
    import, none
    implicit none
    integer :: arg
    print *, arg
    x = 1              ! { dg-error "F2018: C8102" }
  end

! Error messages say it all.
  subroutine sub4 (arg)
    import, only : y
    implicit none
    integer :: arg
    print *, arg
    x = 1              ! { dg-error "F2018: C8102" }
    y = 2
    print *, x         ! { dg-error "F2018: C8102" }
  end

! IMPORT eos and IMPORT, ALL must be unique in the scope.
  subroutine sub5a (arg)
    import, all
    import             ! { dg-error "F2018: C8100" }
    implicit none
    real :: arg
    real :: x          ! { dg-error "F2018: C8102" }
  end

  subroutine sub5b (arg)
    import, only : x
    implicit none
    real :: arg
    real :: x          ! { dg-error "F2018: C8102" }
  end

! Error message says it all.
  integer function func1 ()
    import, only : x
    func1 = x * y      ! { dg-error "F2018: C8102" }
  end

! Error messages say it all.
  subroutine sub6 (arg)
    import, only : func1
    import, only : func2
    import, only : foobar                ! { dg-error "has no IMPLICIT type" }
    implicit none
    integer :: arg
    arg = func1 () * func2 () * func3 () ! { dg-error "F2018: C8102" }
  end

! Error message says it all.
  integer function func2 ()
    use N
    import, none
    implicit none
    func2 = y          ! { dg-error "F2018: C8102" }
  end

! OK
  integer function func3 ()
    func3 = 42
  end

  subroutine sub7 (arg)
    implicit none
    integer :: arg
! OK
    block
       import, only : arg, func1, func2, func3
       arg = func1 () * func2 () * func3 ()
    end block
    block
       arg = func1 ()
       import, only : arg, func1   ! { dg-error "Unexpected IMPORT statement" }
    end block
  end

! Error messages say it all.
  subroutine sub8 (arg)
    implicit none
    integer :: arg
    block
       import, only : func1
       import, only : func2
       import, only : foobar                ! { dg-error "has no IMPLICIT type" }
       arg = func1 () * func2 () * func3 () ! { dg-error "F2018: C8102" }
    end block
  end

! ASSOCIATE does not have a specification part so IMPORT cannot appear.
  subroutine sub9 (arg)
    implicit none
    integer :: arg
    associate (f3 => func3 ())              ! { dg-error "F2018: C8102" }
       import, only : arg, func1            ! { dg-error "Unexpected IMPORT statement" }
       arg = func1 () * func2 () * f3       ! { dg-error "F2018: C8102" }
    end associate
  end

! OK
  subroutine sub10 (arg)
    import, only : t
    implicit none
    type(t) :: arg, mytype
    mytype%i = 1
    arg = mytype
  end

! TYPE t does not appear in the IMPORT list
  subroutine sub11 (arg)
    import, only : progtype
    implicit none
    type(t) :: arg
    progtype%i = 1   ! { dg-error "F2018: C8102" }
    arg = progtype   ! { dg-error "F2018: C8102" }
  end

! TYPE t is excluded by IMPORT, NONE
  subroutine sub12 (arg)
    import, none
    implicit none
    type(t) :: arg, mytype
    mytype%i = 1     ! { dg-error "F2018: C8102" }
    arg = mytype     ! { dg-error "F2018: C8102" }
  end

! TYPE t does not appear in the IMPORT list
  subroutine sub13 (arg)
    import, only : progclass
    implicit none
    class(t) :: arg
    type(t) :: ca(2) = [t(1), t(2)]  ! { dg-error "F2018: C8102" }
    progclass%i = t(1) ! { dg-error "F2018: C8102" }
    arg = progclass    ! { dg-error "F2018: C8102" }
    ca = [t(1), t(2)]  ! { dg-error "has no IMPLICIT type|F2018: C8102" }
    arg = ca(2)        ! Note: The preceeding line catches 'ca' having no implicit type.
  end

! TYPE t is excluded by IMPORT, NONE
  subroutine sub14 (arg)
    import, none
    implicit none
    class(t) :: arg
    class(t), allocatable ::  myclass
    myclass%i =  t(1)  ! { dg-error "F2018: C8102" }
    arg%i = myclass%i  ! { dg-error "F2018: C8102" }
    select type (arg)  ! { dg-error "F2018: C8102" }
      type is (t)
        arg%i = arg%i + 1
      type is (s)
        arg%j = -1
    end select
  end

! TYPE s does not appear in the IMPORT, ONLY list
  subroutine sub15 (arg)
    import, only : t
    implicit none
    class(t) :: arg
    class(t), allocatable ::  myclass
    myclass =  t(1)
    arg%i = myclass%i
    select type (arg)  ! { dg-error "F2018: C8102" }
      type is (t)
        arg%i = arg%i + 1
      type is (s)
        arg%j = -1     ! s is caught at the SELECT TYPE statement
    end select
  end

! This is OK
  subroutine sub16 (arg)
    import, only : t, s
    implicit none
    class(t) :: arg
    class(t), allocatable ::  myclass
    myclass =  t(1)
    arg%i = myclass%i
    select type (arg)
      type is (t)
        arg%i = arg%i + 1
      type is (s)
        arg%j = -1
    end select
  end

  subroutine sub17 (arg)
    import, only : t
    implicit none
    class(t) :: arg
    call sub16 (arg)  ! { dg-error "F2018: C8102" }
  end

! Make sure that recursive procedures do not require the procedure itself to be imported.
  recursive subroutine sub18 (arg)
    import, none
    implicit none
    integer :: arg
    if (arg <= 0) call sub18 (arg)
    arg = 1
  end

  recursive integer function func4 (arg) result (res)
    import, none
    implicit none
    integer :: arg
    if (arg <= 0) arg = func4 (arg)
    res = 1
  end
end
