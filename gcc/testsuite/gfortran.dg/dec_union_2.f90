! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test basic UNION implementation.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

! Empty union
structure /s0/
  union ! U0
    map ! M0
    end map
    map ! M1
    end map
  end union
end structure

! Basic unions
structure /s1/
  union ! U1
    map ! M2
      integer(4) a
    end map
    map ! M3
      real(4) b
    end map
  end union
end structure

structure /s2/
  union ! U2
    map ! M4
      integer(2) w1, w2
    end map
    map ! M5
      integer(4) long
    end map
  end union
end structure

record /s1/ r1
record /s2/ r2

! Basic unions
r1.a = 0
r1.b = 1.33e7
if ( r1.a .eq. 0 ) call aborts ("basic union 1")

! Endian-agnostic runtime check
r2.long = int(z'12345678')
if (.not. (     (r2.w1 .eq. int(z'1234',2) .and. r2.w2 .eq. int(z'5678',2)) &
           .or. (r2.w1 .eq. int(z'5678',2) .and. r2.w2 .eq. int(z'1234',2))) ) then
    call aborts ("basic union 2")
endif

end
