! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test UNIONs with initializations.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
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
      integer(2) a, b, c
    end map
  end union
end structure

record /s3/ r3

! Initialized unions
if ( r3.x .ne. 1600 .or. r3.y .ne. 1800) then
  r3.x = r3.y ! If r3 isn't used the initializations are optimized out
  call aborts ("union initialization")
endif

end
