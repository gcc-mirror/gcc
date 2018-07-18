! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Contributed by Reinhold Bader
!

program pmup
  implicit none
  type t
    integer :: b, a
  end type t

  CLASS(*), allocatable :: a(:)[:]
  integer :: ii

  !! --- ONE --- 
  allocate(real :: a(3)[*])
  IF (this_image() == num_images()) THEN
    SELECT TYPE (a)
      TYPE IS (real)
      a(:)[1] = 2.0
    END SELECT
  END IF
  SYNC ALL

  IF (this_image() == 1) THEN
    SELECT TYPE (a)
      TYPE IS (real)
        IF (ALL(A(:)[1] == 2.0)) THEN
          !WRITE(*,*) 'OK'
        ELSE
          WRITE(*,*) 'FAIL'
          STOP 1
        END IF
      TYPE IS (t)
        ii = a(1)[1]%a
        STOP 2
      CLASS IS (t)
        ii = a(1)[1]%a
        STOP 3
    END SELECT
  END IF

  !! --- TWO --- 
  deallocate(a)
  allocate(t :: a(3)[*])
  IF (this_image() == num_images()) THEN
    SELECT TYPE (a)
      TYPE IS (t)
      a(:)[1]%a = 4.0
    END SELECT
  END IF
  SYNC ALL

  IF (this_image() == 1) THEN
    SELECT TYPE (a)
   TYPE IS (real)
      ii = a(1)[1]
      STOP 4
    TYPE IS (t)
      IF (ALL(A(:)[1]%a == 4.0)) THEN
        !WRITE(*,*) 'OK'
      ELSE
        WRITE(*,*) 'FAIL'
        STOP 5
      END IF
    CLASS IS (t)
      ii = a(1)[1]%a
      STOP 6
    END SELECT
  END IF
end program
