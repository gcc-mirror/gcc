! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test UNIONs with array components.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
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
r5.a(1) = z'41'
r5.a(2) = z'42'
r5.a(3) = z'43'
r5.a(4) = z'44'
r5.a(5) = z'45'
if (     r5.s(1) .ne. 'A' &
    .or. r5.s(2) .ne. 'B' &
    .or. r5.s(3) .ne. 'C' &
    .or. r5.s(4) .ne. 'D' &
    .or. r5.s(5) .ne. 'E') then
  call aborts ("arrays")
endif

end
