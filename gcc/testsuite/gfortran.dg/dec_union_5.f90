! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test UNIONs with array components.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

! Unions with arrays
structure /s5/
  union
    map
      character :: s(5)
    end map
    map
      integer(1) :: a(5)
    end map
  end union
end structure

record /s5/ r5

! Unions with arrays
r5.a(1) = int(z'41',1)
r5.a(2) = int(z'42',1)
r5.a(3) = int(z'43',1)
r5.a(4) =int( z'44',1)
r5.a(5) = int(z'45',1)
if (     r5.s(1) .ne. 'A' &
    .or. r5.s(2) .ne. 'B' &
    .or. r5.s(3) .ne. 'C' &
    .or. r5.s(4) .ne. 'D' &
    .or. r5.s(5) .ne. 'E') then
  call aborts ("arrays")
endif

end
