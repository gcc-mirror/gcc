! { dg-do run }
! { dg-options "-fdec-structure -finit-derived -finit-local-zero" }
!
! Test a UNION with explicit initialization and -finit-derived.
!

subroutine sub
  structure /s2/
    integer(4) :: i = 8
    union ! U7
      map
        integer(4) :: x = 1600
        integer(4) :: y = 1800
      end map
      map
        integer(2) a, b, c, d, e, f, g, h
      end map
    end union
  end structure
  record /s2/ r2

  ! Initialized unions
  if ( r2.i .ne. 8 ) then
    print *, 'structure init'
    STOP 1
  endif

  ! Explicit initializations
  if ( r2.x .ne. 1600 .or. r2.y .ne. 1800) then
    r2.x = r2.y
    print *, 'union explicit init'
    STOP 2
  endif

  ! Initialization from -finit-derived
  if ( r2.h .ne. 0 ) then
    r2.h = 135
    print *, 'union default init'
    STOP 3
  endif

end subroutine

! Initialization expressions
structure /s3/
  integer(4) :: i = 8
  union ! U7
    map
      integer(4) :: x = 1600
      integer(4) :: y = 1800
    end map
    map
      integer(2) a, b, c, d, e
    end map
  end union
end structure

record /s3/ r3

! Initialized unions
if ( r3.i .ne. 8 ) then
  print *, 'structure init'
  STOP 4
endif

! Explicit initializations
if ( r3.x .ne. 1600 .or. r3.y .ne. 1800) then
  r3.x = r3.y
  print *, 'union explicit init'
  STOP 5
endif

! Initialization from -finit-derived
if ( r3.e .ne. 0 ) then
  r3.e = 135
  print *, 'union default init'
  STOP 6
endif

end
