! { dg-do compile }
!
! PR 46459: ICE (segfault): Invalid read in compare_actual_formal [error recovery]
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

  call sub (1)
contains
  subroutine sub (j)
    integer, volatile :: j
  end subroutine
end

subroutine sub1 ()
  call sub2 (1)         ! { dg-error "Explicit interface required" }
end subroutine

subroutine sub2 (j)
  integer, volatile :: j
end subroutine

subroutine sub3 ()
  interface
     subroutine sub2 (j)
       integer, volatile :: j
     end subroutine
  end interface
  call sub2 (1)
end subroutine
