! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test whether union backend declarations are corrently _not_ copied when they
! are not in fact equal. The structure defined in sub() is seen later, but
! where siz has a different value.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
end subroutine

subroutine sub ()
  integer, parameter :: siz = 1024
  structure /s6/
    union ! U0
      map ! M0
        integer ibuf(siz)
      end map
      map ! M1
        character(8) cbuf(siz)
      end map
      map ! M2
        real rbuf(siz)
      end map
    end union
  end structure
  record /s6/ r6
  r6.ibuf(1) = z'badbeef'
  r6.ibuf(2) = z'badbeef'
end subroutine

! Repeat definition from subroutine sub with different size parameter.
! If the structure definition is copied here the stack may get messed up.
integer, parameter :: siz = 65536
structure /s6/
  union ! U12
    map
      integer ibuf(siz)
    end map
    map
      character(8) cbuf(siz)
    end map
    map
      real rbuf(siz)
    end map
  end union
end structure

record /s6/ r6
integer :: r6_canary = 0

! Copied type declaration - this should not cause problems
i = 1
do while (i < siz)
  r6.ibuf(i) = z'badbeef'
  i = i + 1
end do

if ( r6_canary .ne. 0 ) then
  call aborts ('copied decls: overflow')
endif

end
