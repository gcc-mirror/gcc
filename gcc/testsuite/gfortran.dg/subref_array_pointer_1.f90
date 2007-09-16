! { dg-do run }
! Test the fix for PRs29396, 29606, 30625 and 30871, in which pointers
! to arrays with subreferences did not work.
!
  call pr29396
  call pr29606
  call pr30625
  call pr30871
contains
  subroutine pr29396
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
    CHARACTER(LEN=2), DIMENSION(:), POINTER :: a 
    CHARACTER(LEN=4), DIMENSION(3), TARGET :: b 
    b=(/"bbbb","bbbb","bbbb"/) 
    a=>b(:)(2:3) 
    a="aa" 
    IF (ANY(b.NE.(/"baab","baab","baab"/))) CALL ABORT() 
  END subroutine

  subroutine pr29606
! Contributed by Daniel Franke <franke.daniel@gmail.com> 
    TYPE foo
      INTEGER :: value
    END TYPE
    TYPE foo_array
      TYPE(foo), DIMENSION(:), POINTER :: array
    END TYPE
    TYPE(foo_array)                :: array_holder
    INTEGER, DIMENSION(:), POINTER :: array_ptr
    ALLOCATE( array_holder%array(3) )
    array_holder%array = (/ foo(1), foo(2), foo(3) /)
    array_ptr => array_holder%array%value
    if (any (array_ptr .ne. (/1,2,3/))) call abort ()
  END subroutine

  subroutine pr30625
! Contributed by Paul Thomas <pault@gcc.gnu.org> 
    type :: a
      real :: r = 3.14159
      integer :: i = 42
    end type a
    type(a), target :: dt(2)
    integer, pointer :: ip(:)
    ip => dt%i
    if (any (ip .ne. 42)) call abort ()
  end subroutine

  subroutine pr30871
! Contributed by Joost VandeVondele <jv244@cam.ac.uk> 
    TYPE data
      CHARACTER(LEN=3) :: A
    END TYPE
    TYPE(data), DIMENSION(10), TARGET :: Z
    CHARACTER(LEN=1), DIMENSION(:), POINTER :: ptr
    Z(:)%A="123"
    ptr=>Z(:)%A(2:2)
    if (any (ptr .ne. "2")) call abort ()
  END subroutine
end
