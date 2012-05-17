! { dg-do compile }
! { dg-options "-fwhole-program  --param ggc-min-expand=0 --param ggc-min-heapsize=0" }
!
! PR fortran/45087
!

module INTS
  interface
    subroutine NEXT
    end subroutine NEXT
    subroutine VALUE()
    end subroutine VALUE
  end interface
end module INTS

subroutine NEXT
end subroutine NEXT

subroutine VALUE()
  use INTS, only: NEXT
  CALL NEXT
end subroutine VALUE

end
